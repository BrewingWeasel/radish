import gleam/io
import gleam/result
import gleam/erlang
import parser
import interpreter/shell

// TODO: try_map throughout

pub fn main() {
  let assert Ok(cur_shell) = shell.new()
  run_shell(cur_shell)
}

fn run_shell(cur_shell) {
  erlang.get_line("> ")
  |> result.unwrap("")
  |> parser.parse_expression()
  |> result.map(fn(p) { shell.run_command(cur_shell, p.value) })
  |> io.debug()
  run_shell(cur_shell)
}
