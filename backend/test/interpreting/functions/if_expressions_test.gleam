import parser
import interpreter
import gleeunit/should
import interpreter/shell

pub fn single_if_expression_from_string_1_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(if (== 2 2) [(echo hi)])"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.Void)
}

pub fn ifelse_expression_from_string_1_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "
(if-else (== 2 2) [
    (echo hi)
    1
] [
   (echo bye)
   2
])"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(1))
}

pub fn ifelse_expression_from_string_2_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "
(if-else (== 1 2) [
    1
] [
   2
])"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(2))
}
