import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/port.{type Port}
import gleam/string
import interpreter.{type RuntimeError, type State, type Value}
import interpreter/expression
import parser

const timeout: Int = 5_000_000

// TODO: what should the timeout be?

pub fn new(
  port: Port,
  response_port: String,
  shell_process_handler,
) -> Result(Subject(Message), actor.StartError) {
  actor.start(
    interpreter.new_state(port, response_port, shell_process_handler),
    handle_message,
  )
}

pub fn run_command(state: Subject(Message), contents: String) -> String {
  string.inspect(actor.call(state, RunCommand(contents, _), timeout))
}

pub fn run_file(state: Subject(Message), contents: String) -> String {
  string.inspect(actor.call(state, ReadFile(contents, _), timeout))
}

pub fn close(state: Subject(Message)) -> Nil {
  actor.send(state, Kill)
}

pub type Message {
  RunCommand(String, reply_with: Subject(Result(Value, RuntimeError)))
  ReadFile(String, reply_with: Subject(Result(Value, RuntimeError)))
  Kill
}

fn handle_message(message: Message, state: State) -> actor.Next(Message, State) {
  case message {
    RunCommand(contents, client) -> {
      handle_run_command(contents, client, state)
    }
    ReadFile(contents, client) -> {
      let state = handle_run_file(contents, client, state)
      actor.continue(state)
    }
    Kill -> actor.Stop(process.Normal)
  }
}

fn handle_run_file(contents: String, client, state: State) {
  case parser.parse_expression(contents) {
    Ok(ast) -> {
      let response = expression.run_expression(state, ast.value)
      handle_run_file(ast.remaining, client, response.with)
    }
    Error(parser.ExpectedEOF) -> {
      actor.send(client, Ok(interpreter.Void))
      state
    }
    Error(e) -> {
      actor.send(
        client,
        Error(interpreter.InvalidSyntax(interpreter.Parsing(e))),
      )
      state
    }
  }
}

fn handle_run_command(contents, client, state) {
  case parser.parse_expression(contents) {
    Ok(ast) -> {
      let response = expression.run_expression(state, ast.value)
      actor.send(client, response.returned)
      actor.continue(response.with)
    }
    Error(e) -> {
      actor.send(
        client,
        Error(interpreter.InvalidSyntax(interpreter.Parsing(e))),
      )
      // TODO: handle parsing errors
      actor.continue(state)
    }
  }
}
