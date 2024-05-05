use clap::Parser;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{error::ReadlineError, Editor};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
use std::collections::HashMap;
use std::fs::read;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::Arc;
use std::time::Duration;
use tokio::net::UnixDatagram;
use tokio::sync::mpsc;
use tokio::time::sleep;

use uuid::Uuid;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File to execute
    #[arg()]
    file: Option<PathBuf>,

    /// Pass in specific id for shell (defaults to a uuid)
    #[arg(long)]
    shell_id: Option<String>,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let uuid = &args.shell_id.unwrap_or_else(|| Uuid::new_v4().to_string());

    let h = InputValidator {
        brackets: MatchingBracketValidator::new(),
        highlighter: MatchingBracketHighlighter::new(),
    };
    let mut rl = Editor::new().unwrap();
    rl.set_helper(Some(h));

    {
        let radish_main = UnixDatagram::unbound().unwrap();
        radish_main.connect("/tmp/radish/main").unwrap();
        radish_main.send(uuid.as_bytes()).await.unwrap();
    }

    let response_sock = UnixDatagram::bind(format!("/tmp/radish/{uuid}_response")).unwrap();
    let request_sock = Arc::new(UnixDatagram::unbound().unwrap());
    sleep(Duration::from_millis(1)).await;
    loop {
        if request_sock
            .connect(format!("/tmp/radish/{uuid}_request"))
            .is_ok()
        {
            break;
        }
        sleep(Duration::from_millis(5)).await;
    }

    if let Some(file) = args.file {
        request_sock.send(&read(file).unwrap()).await.unwrap();
        // loop {
        //     if handle_response(Arc::clone(&request_sock), &response_sock, &mut processes).await
        //         == ResponseHandled::ReturnValue
        //     {
        //         break;
        //     }
        // }
    } else {
        request_sock.send("i".as_bytes()).await.unwrap();
        let mut pid = vec![0; 8];
        response_sock.recv(&mut pid).await.unwrap();

        println!("PID is {:?}", pid);
        let (tx_get_from_pid, rx_get_from_pid) = mpsc::channel(5);
        let (tx_pid_response, mut rx_pid_response) = mpsc::channel(5);
        let response_handler = ResponseHandler {
            rx_get_from_pid,
            tx_pid_response,
            response_sock,
            request_sock: Arc::clone(&request_sock),
            shell_processes: HashMap::new(),
        };

        tokio::spawn(async move {
            handle_socket_data(response_handler).await;
        });

        let mut is_first = true;
        loop {
            match rl.readline(">> ") {
                Ok(buffer) => {
                    let mut request = pid.clone();
                    request.extend_from_slice(buffer.as_bytes());
                    request_sock.send(&request).await.unwrap();

                    // Wait for process handler to create a function for the specific pid
                    if is_first {
                        sleep(Duration::from_millis(100)).await;
                    }

                    tx_get_from_pid.send(pid.clone()).await.unwrap();
                    if rx_pid_response.recv().await.unwrap().unwrap() == ResponseHandled::Command {
                        // response_sock.recv(&mut []).unwrap();
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    println!("Closing shell");
                    break;
                }
                x => {
                    println!("Event: {:?}", x);
                }
            }
            is_first = false;
        }
    }
}

#[derive(PartialEq, Debug)]
enum ResponseHandled {
    Command,
    ReturnValue,
}

#[derive(Debug)]
struct ShellProcessDetails {
    tx_request: mpsc::Sender<Response>,
    rx_return: mpsc::Receiver<ResponseHandled>,
}

type Pid = Vec<u8>;

#[derive(Debug)]
struct Response {
    bytes: Vec<u8>,
    size: usize,
}

#[derive(Debug)]
struct ResponseHandler {
    rx_get_from_pid: mpsc::Receiver<Pid>,
    tx_pid_response: mpsc::Sender<Option<ResponseHandled>>,
    response_sock: UnixDatagram,
    request_sock: Arc<UnixDatagram>,
    shell_processes: HashMap<Pid, ShellProcessDetails>,
}

async fn handle_socket_data(mut response_handler: ResponseHandler) -> Response {
    loop {
        let mut buf = vec![0; 1000];

        tokio::select! {
            Some(pid) = response_handler.rx_get_from_pid.recv() => {
                let response = match response_handler.shell_processes.get_mut(&pid) {
                    Some(process) => process.rx_return.recv().await,
                    None => None
                };
                response_handler.tx_pid_response.send(response).await.unwrap();
            }
            Ok(size) = response_handler.response_sock.recv(buf.as_mut_slice()) => {
                let (destined_pid, message) = buf.split_at(8);
                let response = Response {
                    size: size - 8,
                    bytes: message.to_vec(),
                };
                let new_request_sender = Arc::clone(&response_handler.request_sock);
                let sender = &response_handler
                    .shell_processes
                    .entry(destined_pid.to_vec())
                    .or_insert_with(|| {
                        let (tx_request, rx_request) = mpsc::channel(5);
                        let (tx_return, rx_return) = mpsc::channel(5);
                        let pid = destined_pid.to_vec();
                        tokio::spawn(async move {
                            run_process(rx_request, tx_return, new_request_sender, pid).await;
                        });
                        ShellProcessDetails {
                            tx_request,
                            rx_return,
                        }
                    })
                    .tx_request;
                sender.send(response).await.unwrap();
            }
        }
        // let size = response_handler
        //     .response_sock
        //     .recv(buf.as_mut_slice())
        //     .await
        //     .unwrap();
    }
}

async fn run_process(
    mut socket_input: mpsc::Receiver<Response>,
    return_message: mpsc::Sender<ResponseHandled>,
    request_sock: Arc<UnixDatagram>,
    pid: Pid,
) {
    loop {
        let sent = socket_input.recv().await.unwrap();
        let response = std::str::from_utf8(&sent.bytes[..sent.size]).unwrap();

        match response.split_at(1) {
            ("c", command) => {
                let mut cur_command = command.to_owned();
                let mut commands = Vec::new();
                'commands: loop {
                    let mut args = Vec::new();
                    loop {
                        let details_sent = socket_input.recv().await.unwrap();
                        match std::str::from_utf8(&details_sent.bytes[..details_sent.size])
                            .unwrap()
                            .split_at(1)
                        {
                            ("a", arg) => {
                                args.push(arg.to_owned());
                            }
                            ("e", _) => {
                                commands.push((cur_command, args));
                                break 'commands;
                            }
                            ("c", new_command) => {
                                commands.push((cur_command, args));
                                cur_command = new_command.to_owned();
                                continue 'commands;
                            }
                            _ => panic!("unexpected message!"),
                        }
                    }
                }
                let mut children: Vec<Child> = Vec::new();
                for (i, (command, args)) in commands.iter().enumerate() {
                    let stdin = if let Some(last) = children.pop() {
                        Stdio::from(last.stdout.unwrap())
                    } else {
                        Stdio::inherit()
                    };

                    let stdout = if i == (commands.len() - 1) {
                        Stdio::inherit()
                    } else {
                        Stdio::piped()
                    };

                    children.push(
                        Command::new(command)
                            .args(args)
                            .stdin(stdin)
                            .stdout(stdout)
                            .spawn()
                            .unwrap(),
                    )
                }

                children.last_mut().unwrap().wait().unwrap();
                {
                    let mut request = pid.clone();
                    request.extend_from_slice("done".as_bytes());
                    request_sock.send(&request).await.unwrap();
                }
                return_message.send(ResponseHandled::Command).await.unwrap();
            }
            ("r", return_value) => {
                println!("returned {}", return_value);
                return_message
                    .send(ResponseHandled::ReturnValue)
                    .await
                    .unwrap();
            }
            v => panic!("Received unexpected message: {}{}", v.0, v.1),
        }
    }
}
