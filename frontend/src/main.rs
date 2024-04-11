use nu_ansi_term::{Color, Style};
use reedline::{DefaultHinter, DefaultPrompt, FileBackedHistory, Reedline, Signal};
use std::{os::unix::net::UnixDatagram, thread::sleep, time::Duration};
use uuid::Uuid;

const HISTORY_FILE_LIMIT: usize = 5000;

fn main() {
    let mut line_editor = Reedline::create().with_hinter(Box::new(
        DefaultHinter::default().with_style(Style::new().italic().fg(Color::Magenta)),
    ));

    if let Some(data_dir) = dirs::data_dir() {
        let history_manager = Box::new(FileBackedHistory::with_file(
            HISTORY_FILE_LIMIT,
            data_dir.join("radish_history"),
        ))
        .expect("to be able to open history");
        line_editor = line_editor.with_history(Box::new(history_manager));
    }

    let prompt = DefaultPrompt::default();

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
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(buffer)) => run_command(buffer, &request_sock, &response_sock),
            Ok(Signal::CtrlD) | Ok(Signal::CtrlC) => {
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
    // let mut buffer = String::new();
    // io::stdin().read_line(&mut buffer)?;
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
                break;
            }
            _ => unreachable!(),
        }
    }
}
