import gleam/otp/actor
import gleam/otp/port.{type Port}
import gleam/erlang/process.{type Subject}
import gleam/string
import gleam/io
import parser
import interpreter.{type RuntimeError, type State, type Value}
import interpreter/expression

const timeout: Int = 5_000_000

// TODO: what should the timeout be?

pub fn new(port: Port) -> Result(Subject(Message), actor.StartError) {
  actor.start(#(port, interpreter.new_state()), handle_message)
}

pub fn run_command(state: Subject(Message), contents: String) -> String {
  string.inspect(actor.call(state, RunCommand(contents, _), timeout))
}

pub fn close(state: Subject(Message)) -> Nil {
  actor.send(state, Kill)
}

pub type Message {
  RunCommand(String, reply_with: Subject(Result(Value, RuntimeError)))
  Kill
}

fn handle_message(
  message: Message,
  state: #(Port, State),
) -> actor.Next(Message, #(Port, State)) {
  case message {
    RunCommand(contents, client) -> {
      handle_run_command(contents, client, state.0, state.1)
    }
    Kill -> actor.Stop(process.Normal)
  }
}

fn handle_run_command(contents, client, port, state) {
  io.print(contents)
  case parser.parse_expression(contents) {
    Ok(ast) -> {
      io.debug(ast)
      let response = expression.run_expression(state, ast.value)
      actor.send(client, response.returned)
      actor.continue(#(port, response.with))
    }
    Error(e) -> {
      actor.send(
        client,
        Error(interpreter.InvalidSyntax(interpreter.Parsing(e))),
      )
      // TODO: handle parsing errors
      actor.continue(#(port, state))
    }
  }
}
