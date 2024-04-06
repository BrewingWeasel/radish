import parser
import interpreter
import gleeunit/should
import interpreter/shell

pub fn single_plus_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(+ 2 3)"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(5))
}

pub fn multiple_plus_1_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(+ 1 1 1 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(4))
}

pub fn multiple_plus_2_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(+ 1 1 (+ 1 3) 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(7))
}

pub fn multiple_multiplication_1_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(* 1 2 3)"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(6))
}

pub fn multiple_multiplication_2_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(* 1 (+ 1 3) 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(4))
}

pub fn multiple_subtraction_2_test() {
  let assert Ok(test_shell) = shell.new()
  let parsed =
    "(- 5 (+ 1 3) 1)"
    |> parser.parse_expression()
    |> should.be_ok()

  shell.run_command(test_shell, parsed.value)
  |> should.be_ok()
  |> should.equal(interpreter.RadishInt(0))
}
