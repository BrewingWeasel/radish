import process_handler
import radish_shell

pub fn main() {
  generate_server()
}

@external(erlang, "socket_connections", "generate_server")
fn generate_server() -> Nil

pub fn start_shell(port, response_port) {
  let assert Ok(handler) = process_handler.new(port)
  #(radish_shell.new(port, response_port, handler), handler)
}
