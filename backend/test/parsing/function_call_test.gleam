import gleeunit/should
import parser

pub fn parse_call_from_expression_test() {
  "(echo hi)"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call(
      [parser.UnquotedStr("echo"), parser.UnquotedStr("hi")],
      piped: False,
    ),
  ))
}

pub fn parse_call_from_expression_2_test() {
  "(echo hi there bro)"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call(
      [
        parser.UnquotedStr("echo"),
        parser.UnquotedStr("hi"),
        parser.UnquotedStr("there"),
        parser.UnquotedStr("bro"),
      ],
      piped: False,
    ),
  ))
}

pub fn parse_piped_call_from_expression_test() {
  "|(echo hi)"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call(
      [parser.UnquotedStr("echo"), parser.UnquotedStr("hi")],
      piped: True,
    ),
  ))
}

pub fn parse_call_from_expression_3_test() {
  "    (echo)"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call([parser.UnquotedStr("echo")], piped: False),
  ))
}
