use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{error::ReadlineError, Editor};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
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

fn main() {
    let h = InputValidator {
        brackets: MatchingBracketValidator::new(),
        highlighter: MatchingBracketHighlighter::new(),
    };
    let mut rl = Editor::new().unwrap();
    rl.set_helper(Some(h));

    let uuid = &Uuid::new_v4().to_string();
    {
        let radish_main = UnixDatagram::unbound().unwrap();
        radish_main.connect("/tmp/radish/main").unwrap();
        radish_main.send(uuid.as_bytes()).unwrap();
    }

    let response_sock = UnixDatagram::bind(format!("/tmp/radish/{uuid}_response")).unwrap();
    sleep(Duration::from_millis(10)); // literally 1 millisecond delay on my computer; sleep a bit longer to be safe
    let request_sock = UnixDatagram::unbound().unwrap();
    request_sock
        .connect(format!("/tmp/radish/{uuid}_request"))
        .unwrap();

    loop {
        match rl.readline(">> ") {
            Ok(buffer) => run_command(buffer, &request_sock, &response_sock),
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

fn run_command(command: String, request_sock: &UnixDatagram, response_sock: &UnixDatagram) {
    request_sock.send(command.as_bytes()).unwrap();

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
            response_sock.recv(&mut []).unwrap();
        }
        ("r", return_value) => {
            println!("returned {}", return_value);
        }
        _ => unreachable!(),
    }
}
