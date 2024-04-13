use async_std::task;
use rustyline::{error::ReadlineError, Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
use std::sync::Arc;
use std::{
    io::stdin,
    os::unix::net::UnixDatagram,
    thread::sleep,
    time::Duration,
};
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
    let request_sock = Arc::new(UnixDatagram::unbound().unwrap());
    request_sock
        .connect(format!("/tmp/radish/{uuid}_request"))
        .unwrap();

    loop {
        match rl.readline(">> ") {
            Ok(buffer) => {
                task::block_on(async { run_command(buffer, Arc::clone(&request_sock), &response_sock).await });
                println!("done!");
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

async fn run_command(command: String, request_sock: Arc<UnixDatagram>, response_sock: &UnixDatagram) {
    request_sock.send(command.as_bytes()).unwrap();
    let other_request_sock = Arc::clone(&request_sock);
    let task = task::spawn(async move {
        let mut buffer = String::new();
        loop {
            match async_std::io::stdin().read_line(&mut buffer).await {
                Ok(0) => {
                    other_request_sock
                        .send(String::from("e").as_bytes())
                        .unwrap();
                    break;
                }
                Ok(_) => {
                    other_request_sock
                        .send(format!("i{buffer}").as_bytes())
                        .unwrap();
                }
                Err(e) => {
                    println!("{:?}", e);
                    break;
                }
            }
            buffer.clear();
        }
    });
    loop {
        let mut buf = [0; 128];
        response_sock.recv(&mut buf).unwrap();
        let response = std::str::from_utf8(&buf).unwrap();
        match response.split_at(1) {
            ("o", stdout) => {
                print!("{}", stdout);
            }
            ("r", return_value) => {
                println!("returned {}", return_value);
                task.cancel().await;
                break;
            }
            _ => unreachable!(),
        }
    }
}
