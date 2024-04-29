import gleeunit/should
import interpreter
import utils.{type Interpreted, interpreter_command}

pub fn single_plus_test_() {
  interpreter_command("(+ 2 3)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishInt(5))
  })
}

pub fn multiple_plus_1_test_() {
  interpreter_command("(+ 1 1 1 1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishInt(4))
  })
}

pub fn multiple_plus_2_test_() {
  interpreter_command("(+ 1 1 (+ 1 3) 1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishInt(7))
  })
}

pub fn multiple_multiplication_1_test_() {
  interpreter_command("(* 1 2 3)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishInt(6))
  })
}

pub fn multiple_multiplication_2_test_() {
  interpreter_command("(* 1 (+ 1 3) 1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishInt(4))
  })
}

pub fn multiple_subtraction_2_test_() {
  interpreter_command("(- 5 (+ 1 3) 1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishInt(0))
  })
}
