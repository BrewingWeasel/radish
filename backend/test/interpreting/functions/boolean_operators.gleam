import parser
import interpreter
import interpreter/expression
import gleeunit/should

pub fn single_less_than_1_test() {
  let parsed =
    "(< 2 3)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(True))
}

pub fn single_less_than_2_test() {
  let parsed =
    "(< 4 3)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(False))
}

pub fn single_less_than_3_test() {
  let parsed =
    "(< 0 0)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(False))
}

pub fn multiple_less_than_1_test() {
  let parsed =
    "(< 0 1 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(False))
}

pub fn multiple_greater_than_1_test() {
  let parsed =
    "(> 7 6 3 2 1 0)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(True))
}

pub fn multiple_greater_than_2_test() {
  let parsed =
    "(> 7 6 3 2 1 (+ 2 5))"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(False))
}

pub fn single_equal_to_1_test() {
  let parsed =
    "(== 1 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(True))
}

pub fn single_equal_to_2_test() {
  let parsed =
    "(== 1 2)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(False))
}

pub fn multiple_equal_to_1_test() {
  let parsed =
    "   (  == 1    1 1 1 1   1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(True))
}

pub fn multiple_unequal_to_1_test() {
  let parsed =
    "   (  != 1    1 1 1 1   1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(False))
}

pub fn multiple_unequal_to_2_test() {
  let parsed =
    "   (  != 2    1 2 1 2   1)"
    |> parser.parse_expression()
    |> should.be_ok()

  expression.run_expression(interpreter.new_state(), parsed.value).returned
  |> should.be_ok()
  |> should.equal(interpreter.RadishBool(True))
}
