import gleam/otp/actor
import gleam/erlang/process.{type Subject}
import parser
import interpreter.{type RuntimeError, type Value}
import interpreter/expression
import interpreter/state.{type State}

const timeout: Int = 5_000_000

// TODO: what should the timeout be?

pub fn new() -> Result(Subject(Message), actor.StartError) {
  actor.start(state.new(), handle_message)
}

pub fn run_command(
  state: Subject(Message),
  ast: parser.Ast,
) -> Result(Value, RuntimeError) {
  actor.call(state, RunCommand(ast, _), timeout)
}

pub fn close(state: Subject(Message)) -> Nil {
  actor.send(state, Kill)
}

pub type Message {
  RunCommand(parser.Ast, reply_with: Subject(Result(Value, RuntimeError)))
  Kill
}

fn handle_message(message: Message, state: State) -> actor.Next(Message, State) {
  case message {
    RunCommand(ast, client) -> {
      let response = expression.run_expression(state, ast)
      actor.send(client, response.returned)
      actor.continue(response.with)
    }
    Kill -> actor.Stop(process.Normal)
  }
}
