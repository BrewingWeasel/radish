import gleeunit/should
import interpreter
import utils.{type Interpreted, interpreter_command}

pub fn single_less_than_1_test_() {
  interpreter_command("(< 2 3)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(True))
  })
}

pub fn single_less_than_2_test_() {
  interpreter_command("(< 4 3)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(False))
  })
}

pub fn single_less_than_3_test_() {
  interpreter_command("(< 0 0)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(False))
  })
}

pub fn multiple_less_than_1_test_() {
  interpreter_command("(< 0 1 1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(False))
  })
}

pub fn multiple_greater_than_1_test_() {
  interpreter_command("(> 7 6 3 2 1 0)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(True))
  })
}

pub fn multiple_greater_than_2_test_() {
  interpreter_command("(> 7 6 3 2 1 (+ 2 5))", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(False))
  })
}

pub fn single_equal_to_1_test_() {
  interpreter_command("(== 1 1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(True))
  })
}

pub fn single_equal_to_2_test_() {
  interpreter_command("(== 1 2)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(False))
  })
}

pub fn multiple_equal_to_1_test_() {
  interpreter_command("   (  == 1    1 1 1 1   1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(True))
  })
}

pub fn multiple_unequal_to_1_test_() {
  interpreter_command("   (  != 1    1 1 1 1   1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(False))
  })
}

pub fn multiple_unequal_to_2_test_() {
  interpreter_command("   (  != 2    1 2 1 2   1)", fn(parsed: Interpreted) {
    parsed.returned.returned
    |> should.be_ok()
    |> should.equal(interpreter.RadishBool(True))
  })
}
