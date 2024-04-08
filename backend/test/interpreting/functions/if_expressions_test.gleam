import parser
import interpreter
import interpreter/expression
import gleeunit/should

pub fn single_if_expression_from_string_1_test() {
  let parsed =
    "(if (== 2 2) [(echo hi)])"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.Void)
}

pub fn ifelse_expression_from_string_1_test() {
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

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(1))
}

pub fn ifelse_expression_from_string_2_test() {
  let parsed =
    "
(if-else (== 1 2) [
    1
] [
   2
])"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(2))
}
