import parser
import gleeunit/should

pub fn parse_variable_expression_test() {
  "$cat"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.Variable("cat")))
}
