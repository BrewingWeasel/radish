use async_std::io::ReadExt;
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
    let request_sock = UnixDatagram::unbound().unwrap();
    request_sock
        .connect(format!("/tmp/radish/{uuid}_request"))
        .unwrap();

    let stdin_sock = Arc::new(UnixDatagram::unbound().unwrap());
    stdin_sock
        .connect(format!("/tmp/radish/{uuid}_stdin"))
        .unwrap();

    loop {
        match rl.readline(">> ") {
            Ok(buffer) => {
                task::block_on(async { run_command(buffer, &request_sock, Arc::clone(&stdin_sock), &response_sock).await });
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

async fn run_command(command: String, request_sock: &UnixDatagram, stdin_sock: Arc<UnixDatagram>, response_sock: &UnixDatagram) {
    request_sock.send(command.as_bytes()).unwrap();
    let task = task::spawn(async move {
        let mut buffer = Vec::from([0; 1]);
        loop {
            match async_std::io::stdin().read_exact(&mut buffer).await {
                Ok(()) => {
                    stdin_sock
                        .send(&buffer)
                        .unwrap();
                }
                Err(_) => {
                    stdin_sock
                        .send("end".as_bytes())
                        .unwrap();
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
