import gleeunit/should
import parser

pub fn parse_variable_expression_test() {
  "$cat"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.Variable("cat")))
}

pub fn parse_variable_expression_unicode_test() {
  "$žvaigždė"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.Variable("žvaigždė")))
}

pub fn parse_variable_expression_no_variable_test() {
  "$"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.UnquotedStr("$")))
}
