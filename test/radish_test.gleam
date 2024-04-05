import gleeunit
import gleeunit/should
import radish

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn parse_expr1_test() {
  "(echo \"hello there!\")"
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing(
    "",
    radish.Call([radish.UnquotedStr("echo"), radish.StrVal("hello there!")]),
  ))
}

pub fn parse_expr2_test() {
  "         (echo [thing1 thing2 thing3]  )"
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing(
    "",
    radish.Call([
      radish.UnquotedStr("echo"),
      radish.BracketList([
        radish.UnquotedStr("thing1"),
        radish.UnquotedStr("thing2"),
        radish.UnquotedStr("thing3"),
      ]),
    ]),
  ))
}

pub fn parse_call_from_expression_test() {
  "(echo hi)"
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing(
    "",
    radish.Call([radish.UnquotedStr("echo"), radish.UnquotedStr("hi")]),
  ))
}

pub fn parse_call_from_expression_2_test() {
  "    (echo)"
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing("", radish.Call([radish.UnquotedStr("echo")])))
}

pub fn parse_list_test() {
  "echo)"
  |> radish.parse_list(")")
  |> should.be_ok()
  |> should.equal(radish.Parsing("", [radish.UnquotedStr("echo")]))
}

pub fn parse_list_spacing_1_test() {
  "      echo)"
  |> radish.parse_list(")")
  |> should.be_ok()
  |> should.equal(radish.Parsing("", [radish.UnquotedStr("echo")]))
}

pub fn parse_list_spacing_2_test() {
  "      echo   \"sveikas\")"
  |> radish.parse_list(")")
  |> should.be_ok()
  |> should.equal(
    radish.Parsing("", [radish.UnquotedStr("echo"), radish.StrVal("sveikas")]),
  )
}

pub fn parse_list_new_line_ending_test() {
  "      echo   \"sveikas\"
  )"
  |> radish.parse_list(")")
  |> should.be_ok()
  |> should.equal(
    radish.Parsing("", [radish.UnquotedStr("echo"), radish.StrVal("sveikas")]),
  )
}

pub fn parse_list_formatted_test() {
  "[
   \"hi\"
   \"there\"
   \"!!\"
]"
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing(
    "",
    radish.BracketList([
      radish.StrVal("hi"),
      radish.StrVal("there"),
      radish.StrVal("!!"),
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
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing(
    "",
    radish.BracketList([
      radish.StrVal("hi"),
      radish.Call([
        radish.UnquotedStr("get_name_from_list"),
        radish.BracketList([
          radish.UnquotedStr("John"),
          radish.UnquotedStr("Josie"),
          radish.UnquotedStr("Joe"),
          radish.UnquotedStr("Jane"),
        ]),
      ]),
      radish.StrVal("!!"),
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
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing(
    "",
    radish.BracketList([
      radish.StrVal("hi"),
      radish.Call([
        radish.UnquotedStr("get_name_from_list"),
        radish.BracketList([
          radish.UnquotedStr("John"),
          radish.UnquotedStr("Josie"),
          radish.UnquotedStr("Joe"),
          radish.UnquotedStr("Jane"),
        ]),
      ]),
      radish.StrVal("!!"),
    ]),
  ))
}

pub fn fail_parse_list_test() {
  "      echo   \"sveikas\""
  |> radish.parse_list(")")
  |> should.be_error()
  |> should.equal(radish.MissingListEnd(")"))
}

pub fn parse_string_from_expression_test() {
  "\"Labas pasauli!\""
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing("", radish.StrVal("Labas pasauli!")))
}

pub fn parse_string_from_expression_2_test() {
  "   \"   Labas pasauli!\""
  |> radish.parse_expression()
  |> should.be_ok()
  |> should.equal(radish.Parsing("", radish.StrVal("   Labas pasauli!")))
}

pub fn parse_string_escaping_1_test() {
  "Labas \\\" pasauli!\""
  |> radish.parse_string()
  |> should.be_ok()
  |> should.equal(radish.Parsing("", radish.StrVal("Labas \\\" pasauli!")))
}

pub fn parse_string_escaping_2_test() {
  "Labas\\pasauli!\""
  |> radish.parse_string()
  |> should.be_ok()
  |> should.equal(radish.Parsing("", radish.StrVal("Labas\\pasauli!")))
}

pub fn failed_parse_test() {
  "Labas\\pasauli!"
  |> radish.parse_string()
  |> should.be_error()
  |> should.equal(radish.MissingQuote)
}
