import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Ast {
  Call(contents: List(Ast), piped: Bool)
  StrVal(String)
  Variable(String)
  BracketList(List(Ast))
  UnquotedStr(String)
  Number(Int)
}

pub type ParseError {
  MissingQuote
  MissingListEnd(String)
  UnexpectedFileEnd
  ExpectedEOF
}

pub type Parsing(t) {
  Parsing(remaining: String, value: t)
}

pub fn parse_expression(input: String) -> Result(Parsing(Ast), ParseError) {
  case string.trim_left(input) {
    "\"" <> ending -> parse_string(ending)
    "(" <> call -> {
      use list <- result.try(parse_list(call, ")"))
      Ok(Parsing(list.remaining, Call(list.value, piped: False)))
    }
    "|(" <> call -> {
      use list <- result.try(parse_list(call, ")"))
      Ok(Parsing(list.remaining, Call(list.value, piped: True)))
    }
    "[" <> call -> {
      use list <- result.try(parse_list(call, "]"))
      Ok(Parsing(list.remaining, BracketList(list.value)))
    }
    "" -> Error(ExpectedEOF)
    v -> {
      use atom <- result.try(parse_atom(v))
      Ok(Parsing(
        atom.remaining,
        int.parse(atom.value)
          |> result.map(Number)
          |> result.try_recover(fn(_) {
          case string.pop_grapheme(atom.value) {
            Ok(#("$", var_name)) if var_name != "" -> Ok(Variable(var_name))
            _ -> Error(Nil)
          }
        })
          |> result.unwrap(UnquotedStr(atom.value)),
      ))
    }
  }
}

fn parse_atom(input: String) -> Result(Parsing(String), ParseError) {
  case string.pop_grapheme(input) {
    Ok(#(c, rest)) if c == " " || c == "\n" || c == "\t" ->
      Ok(Parsing(rest, ""))
    Ok(#(c, _)) if c == ")" || c == "]" -> Ok(Parsing(input, ""))
    Ok(#(c, rest)) -> {
      use parsed_atom <- result.try(parse_atom(rest))
      Ok(Parsing(parsed_atom.remaining, c <> parsed_atom.value))
    }
    Error(Nil) -> Ok(Parsing("", ""))
  }
}

pub fn parse_list(
  input: String,
  ending: String,
) -> Result(Parsing(List(Ast)), ParseError) {
  use parsed_list <- result.try(do_parse_list(input, ending, []))
  Ok(Parsing(parsed_list.remaining, list.reverse(parsed_list.value)))
}

fn do_parse_list(
  input: String,
  ending: String,
  acc: List(Ast),
) -> Result(Parsing(List(Ast)), ParseError) {
  case string.pop_grapheme(string.trim_left(input)) {
    Ok(#(v, rest)) if v == ending -> Ok(Parsing(rest, acc))
    Ok(_) -> {
      use cur_elem <- result.try(parse_expression(input))
      do_parse_list(cur_elem.remaining, ending, [cur_elem.value, ..acc])
    }
    Error(_) -> Error(MissingListEnd(ending))
  }
}

pub fn parse_string(input: String) -> Result(Parsing(Ast), ParseError) {
  use next_part <- result.try(do_parse_string(string.to_graphemes(input), ""))
  Ok(Parsing(value: StrVal(next_part.value), remaining: next_part.remaining))
}

fn do_parse_string(
  input: List(String),
  acc: String,
) -> Result(Parsing(String), ParseError) {
  case input {
    ["\\", following, ..rest] -> {
      do_parse_string(
        rest,
        acc
          <> case following {
          "n" -> "\n"
          "t" -> "\t"
          "\\" -> "\\"
          x -> "\\" <> x
        },
      )
    }
    ["\"", ..rest] -> {
      Ok(Parsing(string.concat(rest), acc))
    }
    [v, ..rest] -> {
      do_parse_string(rest, acc <> v)
    }
    [] -> Error(MissingQuote)
  }
}
