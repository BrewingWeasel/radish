import parser
import interpreter
import interpreter/expression
import gleeunit/should

pub fn single_plus_test() {
  let parsed =
    "(+ 2 3)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(5))
}

pub fn multiple_plus_1_test() {
  let parsed =
    "(+ 1 1 1 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(4))
}

pub fn multiple_plus_2_test() {
  let parsed =
    "(+ 1 1 (+ 1 3) 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(7))
}

pub fn multiple_multiplication_1_test() {
  let parsed =
    "(* 1 2 3)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(6))
}

pub fn multiple_multiplication_2_test() {
  let parsed =
    "(* 1 (+ 1 3) 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(4))
}

pub fn multiple_subtraction_2_test() {
  let parsed =
    "(- 5 (+ 1 3) 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(0))
}
