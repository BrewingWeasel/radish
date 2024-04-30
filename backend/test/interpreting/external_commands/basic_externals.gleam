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

pub fn simple_echo_2_test_() {
  interpreter_command(
    "(echo \"hello\" there how are you)",
    fn(parsed: Interpreted) {
      parsed.returned.returned
      |> should.be_ok()
      |> should.equal(interpreter.Void)

      parsed.to_frontend
      |> should.equal(
        utils.Command([#("echo", ["hello", "there", "how", "are", "you"])]),
      )
    },
  )
}

pub fn simple_pipe_test_() {
  interpreter_command("(| (echo \"hi\") (cat))", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.Void)

    parsed.to_frontend
    |> should.equal(utils.Command([#("echo", ["hi"]), #("cat", [])]))
  })
}
