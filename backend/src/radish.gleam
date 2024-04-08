import radish_shell

// TODO: try_map throughout

pub fn main() {
  generate_server()
}

@external(erlang, "socket_connections", "generate_server")
fn generate_server() -> Nil

pub fn start_shell(port) {
  radish_shell.new(port)
}
