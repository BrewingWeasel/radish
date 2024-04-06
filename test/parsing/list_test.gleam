import gleeunit/should
import parser

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
