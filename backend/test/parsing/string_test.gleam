import gleeunit/should
import parser

pub fn parse_string_from_expression_test() {
  "\"Labas pasauli!\""
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas pasauli!")))
}

pub fn parse_string_from_expression_2_test() {
  "   \"   Labas pasauli!\""
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("   Labas pasauli!")))
}

pub fn parse_string_from_expression_with_escaped_chars_test() {
  "\"Labas \\\" pasauli!\""
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas \" pasauli!")))
}

pub fn parse_string_from_expression_with_escaped_chars_2_test() {
  "\"Labas \\n pasauli!\""
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas \n pasauli!")))
}

pub fn parse_string_escaping_1_test() {
  "Labas \\\" pasauli!\""
  |> parser.parse_string()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas \" pasauli!")))
}

pub fn parse_string_escaping_2_test() {
  "Labas\\pasauli!\""
  |> parser.parse_string()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas\\pasauli!")))
}

pub fn failed_parse_string_test() {
  "Labas\\pasauli!"
  |> parser.parse_string()
  |> should.be_error()
  |> should.equal(parser.MissingQuote)
}
