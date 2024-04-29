import gleam/erlang/atom
import gleam/otp/port.{type Port}
import gleam/otp/task
import gleeunit/should
import ids/uuid
import interpreter.{type RanExpression}
import interpreter/expression
import parser

pub type Interpreted {
  Interpreted(
    returned: RanExpression(interpreter.Value),
    to_frontend: SentToFrontend,
  )
}

pub type SentToFrontend {
  Response
  Command(List(#(String, List(String))))
}

pub fn run_command_from_string(input: String) -> Interpreted {
  let parsed =
    input
    |> parser.parse_expression()
    |> should.be_ok()

  let assert Ok(id) = uuid.generate_v4()
  let #(request_port, response_port, request_port_name, response_port_name) =
    simulate_shell(id)

  let to_frontend =
    task.async(fn() { get_command(request_port_name, response_port) })
  let ran_expression =
    interpreter.new_state(request_port, response_port_name)
    |> expression.run_expression(parsed.value)

  Interpreted(
    returned: ran_expression,
    to_frontend: task.await_forever(to_frontend),
  )
}

pub fn interpreter_command(command: String, handler: fn(Interpreted) -> Nil) {
  let timeout = atom.create_from_string("timeout")
  #(timeout, 3.0, fn() {
    command
    |> run_command_from_string()
    |> handler()
  })
}

@external(erlang, "utils_ffi", "simulate_shell")
fn simulate_shell(uuid: String) -> #(Port, Port, String, String)

@external(erlang, "utils_ffi", "get_command")
fn get_command(request_port_name: String, response_port: Port) -> SentToFrontend

@external(erlang, "utils_ffi", "setup")
pub fn setup() -> Nil
