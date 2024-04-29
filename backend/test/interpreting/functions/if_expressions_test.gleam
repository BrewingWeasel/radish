import gleeunit/should
import interpreter
import utils.{type Interpreted, interpreter_command}

pub fn single_if_expression_from_string_1_test() {
  interpreter_command("(if (== 2 2) [(echo hi)])", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.Void)
  })
}

pub fn ifelse_expression_from_string_1_test() {
  interpreter_command(
    "
(if-else (== 2 2) [
    (echo hi)
    1
] [
   (echo bye)
   2
])",
    fn(parsed: Interpreted) {
      parsed.returned.returned
      |> should.be_ok()
      |> should.equal(interpreter.RadishInt(1))
    },
  )
}

pub fn ifelse_expression_from_string_2_test() {
  interpreter_command(
    "
(if-else (== 1 2) [
    1
] [
   2
])",
    fn(parsed: Interpreted) {
      parsed.returned.returned
      |> should.be_ok()
      |> should.equal(interpreter.RadishInt(2))
    },
  )
}
