import gleeunit
import gleeunit/should
import parser

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
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
  "    (echo)"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.Call([parser.UnquotedStr("echo")], piped: False),
  ))
}

pub fn parse_list_test() {
  "echo)"
  |> parser.parse_list(")")
  |> should.be_ok()
  |> should.equal(parser.Parsing("", [parser.UnquotedStr("echo")]))
}

pub fn parse_list_spacing_1_test() {
  "      echo)"
  |> parser.parse_list(")")
  |> should.be_ok()
  |> should.equal(parser.Parsing("", [parser.UnquotedStr("echo")]))
}

pub fn parse_list_spacing_2_test() {
  "      echo   \"sveikas\")"
  |> parser.parse_list(")")
  |> should.be_ok()
  |> should.equal(
    parser.Parsing("", [parser.UnquotedStr("echo"), parser.StrVal("sveikas")]),
  )
}

pub fn parse_list_new_line_ending_test() {
  "      echo   \"sveikas\"
  )"
  |> parser.parse_list(")")
  |> should.be_ok()
  |> should.equal(
    parser.Parsing("", [parser.UnquotedStr("echo"), parser.StrVal("sveikas")]),
  )
}

pub fn parse_list_formatted_test() {
  "[
   \"hi\"
   \"there\"
   \"!!\"
]"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.BracketList([
      parser.StrVal("hi"),
      parser.StrVal("there"),
      parser.StrVal("!!"),
    ]),
  ))
}

pub fn parse_list_formatted_2_test() {
  "[
   \"hi\"
   (get_name_from_list [
      John
      Josie
      Joe
      Jane
   ])
   \"!!\"
]"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.BracketList([
      parser.StrVal("hi"),
      parser.Call(
        [
          parser.UnquotedStr("get_name_from_list"),
          parser.BracketList([
            parser.UnquotedStr("John"),
            parser.UnquotedStr("Josie"),
            parser.UnquotedStr("Joe"),
            parser.UnquotedStr("Jane"),
          ]),
        ],
        piped: False,
      ),
      parser.StrVal("!!"),
    ]),
  ))
}

pub fn parse_list_formatted_3_test() {
  "[
   \"hi\"
   (
      get_name_from_list 
      [
         John
         Josie
         Joe
         Jane
      ]
   )
   \"!!\"
]"
  |> parser.parse_expression()
  |> should.be_ok()
  |> should.equal(parser.Parsing(
    "",
    parser.BracketList([
      parser.StrVal("hi"),
      parser.Call(
        [
          parser.UnquotedStr("get_name_from_list"),
          parser.BracketList([
            parser.UnquotedStr("John"),
            parser.UnquotedStr("Josie"),
            parser.UnquotedStr("Joe"),
            parser.UnquotedStr("Jane"),
          ]),
        ],
        piped: False,
      ),
      parser.StrVal("!!"),
    ]),
  ))
}

pub fn fail_parse_list_test() {
  "      echo   \"sveikas\""
  |> parser.parse_list(")")
  |> should.be_error()
  |> should.equal(parser.MissingListEnd(")"))
}

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

pub fn parse_string_escaping_1_test() {
  "Labas \\\" pasauli!\""
  |> parser.parse_string()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas \\\" pasauli!")))
}

pub fn parse_string_escaping_2_test() {
  "Labas\\pasauli!\""
  |> parser.parse_string()
  |> should.be_ok()
  |> should.equal(parser.Parsing("", parser.StrVal("Labas\\pasauli!")))
}

pub fn failed_parse_test() {
  "Labas\\pasauli!"
  |> parser.parse_string()
  |> should.be_error()
  |> should.equal(parser.MissingQuote)
}
