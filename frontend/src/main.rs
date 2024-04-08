use reedline::{DefaultPrompt, Reedline, Signal};
use std::{os::unix::net::UnixDatagram, thread::sleep, time::Duration};
use uuid::Uuid;

fn main() {
    let mut line_editor = Reedline::create();
    let prompt = DefaultPrompt::default();

    let uuid = &Uuid::new_v4().to_string();
    {
        let radish_main = UnixDatagram::unbound().unwrap();
        radish_main.connect("/tmp/radish_main").unwrap();
        radish_main.send(uuid.as_bytes()).unwrap();
    }

    let response_sock = UnixDatagram::bind(format!("/tmp/radish{uuid}_response")).unwrap();
    sleep(Duration::from_millis(50));
    let request_sock = UnixDatagram::unbound().unwrap();
    request_sock
        .connect(format!("/tmp/radish{uuid}_request"))
        .unwrap();

    loop {
        let sig = line_editor.read_line(&prompt);
        match sig {
            Ok(Signal::Success(buffer)) => {
                request_sock.send(buffer.as_bytes()).unwrap();
                let mut buf = [0; 128];
                response_sock.recv(&mut buf).unwrap();
                println!("{}", std::str::from_utf8(&buf).unwrap());
            }
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
