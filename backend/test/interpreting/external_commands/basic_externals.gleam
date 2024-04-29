import gleeunit/should

import interpreter
import utils.{type Interpreted, interpreter_command}

pub fn simple_echo_test_() {
  interpreter_command("(echo \"hello\")", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.Void)

    parsed.to_frontend
    |> should.equal(utils.Command([#("echo", ["hello"])]))
  })
}
