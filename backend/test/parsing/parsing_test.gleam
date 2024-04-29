import gleeunit/should
import parser

pub fn parse_expr1_test() {
  "(echo \"hello there!\")"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call(
      [parser.UnquotedStr("echo"), parser.StrVal("hello there!")],
      piped: False,
    ),
  ))
}

pub fn parse_expr2_test() {
  "         (echo [thing1 thing2 thing3]  )"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call(
      [
        parser.UnquotedStr("echo"),
        parser.BracketList([
          parser.UnquotedStr("thing1"),
          parser.UnquotedStr("thing2"),
          parser.UnquotedStr("thing3"),
        ]),
      ],
      piped: False,
    ),
  ))
}

pub fn parse_expr3_test() {
  "(echo $x)"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call(
      [parser.UnquotedStr("echo"), parser.Variable("x")],
      piped: False,
    ),
  ))
}
