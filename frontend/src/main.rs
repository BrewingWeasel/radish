use clap::Parser;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{error::ReadlineError, Editor};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
use std::fs::read;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::{os::unix::net::UnixDatagram, thread::sleep, time::Duration};
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

fn main() {
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
        radish_main.send(uuid.as_bytes()).unwrap();
    }

    let response_sock = UnixDatagram::bind(format!("/tmp/radish/{uuid}_response")).unwrap();
    let request_sock = UnixDatagram::unbound().unwrap();
    sleep(Duration::from_millis(1));
    loop {
        if request_sock
            .connect(format!("/tmp/radish/{uuid}_request"))
            .is_ok()
        {
            break;
        }
        sleep(Duration::from_millis(5));
    }

    if let Some(file) = args.file {
        request_sock.send(&read(file).unwrap()).unwrap();
        loop {
            if handle_response(&request_sock, &response_sock) == ResponseHandled::ReturnValue {
                break;
            }
        }
    } else {
        request_sock.send("i".as_bytes()).unwrap();
        loop {
            match rl.readline(">> ") {
                Ok(buffer) => {
                    request_sock.send(buffer.as_bytes()).unwrap();
                    if handle_response(&request_sock, &response_sock) == ResponseHandled::Command {
                        response_sock.recv(&mut []).unwrap();
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
        }
    }
}

#[derive(PartialEq)]
enum ResponseHandled {
    Command,
    ReturnValue,
}

fn handle_response(request_sock: &UnixDatagram, response_sock: &UnixDatagram) -> ResponseHandled {
    let mut buf = vec![0; 100];
    let size = response_sock.recv(buf.as_mut_slice()).unwrap();
    let response = std::str::from_utf8(&buf[..size]).unwrap();

    match response.split_at(1) {
        ("c", command) => {
            let mut cur_command = command.to_owned();
            let mut commands = Vec::new();
            'commands: loop {
                let mut args = Vec::new();
                loop {
                    let mut details_buf = vec![0; 100];
                    let size = response_sock.recv(&mut details_buf).unwrap();
                    match std::str::from_utf8(&details_buf[..size])
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
            request_sock.send("done".as_bytes()).unwrap();
            ResponseHandled::Command
        }
        ("r", return_value) => {
            println!("returned {}", return_value);
            ResponseHandled::ReturnValue
        }
        v => panic!("Received unexpected message: {}{}", v.0, v.1),
    }
}
