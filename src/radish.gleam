import gleam/io
import gleam/string
import gleam/result
import gleam/list
import gleam/erlang

pub type Ast {
  Call(List(Ast))
  StrVal(String)
  BracketList(List(Ast))
  UnquotedStr(String)
}

pub fn main() {
  erlang.get_line("> ")
  |> result.unwrap("")
  |> parse_expression()
  |> io.debug()
  main()
}

pub type ParseError {
  MissingQuote
  MissingListEnd(String)
  UnexpectedFileEnd
}

pub type Parsing(t) {
  Parsing(remaining: String, value: t)
}

pub fn parse_expression(input: String) -> Result(Parsing(Ast), ParseError) {
  case string.trim_left(input) {
    "\"" <> ending -> parse_string(ending)
    "(" <> call -> {
      use list <- result.try(parse_list(call, ")"))
      Ok(Parsing(list.remaining, Call(list.value)))
    }
    "[" <> call -> {
      use list <- result.try(parse_list(call, "]"))
      Ok(Parsing(list.remaining, BracketList(list.value)))
    }
    v -> {
      use atom <- result.try(parse_atom(v))
      Ok(Parsing(atom.remaining, UnquotedStr(atom.value)))
    }
  }
}

fn parse_atom(input: String) -> Result(Parsing(String), ParseError) {
  case string.pop_grapheme(input) {
    Ok(#(" ", rest)) -> Ok(Parsing(rest, ""))
    Ok(#(c, _)) if c == ")" || c == "]" -> Ok(Parsing(input, ""))
    Ok(#(c, rest)) -> {
      use parsed_atom <- result.try(parse_atom(rest))
      Ok(Parsing(parsed_atom.remaining, c <> parsed_atom.value))
    }
    Error(Nil) -> Error(UnexpectedFileEnd)
  }
}

fn parse_list(
  input: String,
  ending: String,
) -> Result(Parsing(List(Ast)), ParseError) {
  use parsed_list <- result.try(do_parse_list(input, ending))
  Ok(Parsing(parsed_list.remaining, list.reverse(parsed_list.value)))
}

fn do_parse_list(
  input: String,
  ending: String,
) -> Result(Parsing(List(Ast)), ParseError) {
  case string.pop_grapheme(input) {
    Ok(#(v, rest)) if v == ending -> Ok(Parsing(rest, []))
    Ok(_) -> {
      use cur_elem <- result.try(parse_expression(input))
      use next_items <- result.try(do_parse_list(cur_elem.remaining, ending))
      Ok(Parsing(next_items.remaining, [cur_elem.value, ..next_items.value]))
    }
    Error(_) -> Error(MissingListEnd(ending))
  }
}

fn parse_string(input: String) -> Result(Parsing(Ast), ParseError) {
  use next_part <- result.try(do_parse_string(string.to_graphemes(input)))
  Ok(Parsing(value: StrVal(next_part.value), remaining: next_part.remaining))
}

fn do_parse_string(input: List(String)) -> Result(Parsing(String), ParseError) {
  case input {
    ["\\", following, ..rest] -> {
      use next_part <- result.try(do_parse_string(rest))
      Ok(Parsing(
        next_part.remaining,
        case following {
            "n" -> "\n"
            "t" -> "\t"
            "\\" -> "\\"
            x -> "\\" <> x
          }
          <> next_part.value,
      ))
    }
    ["\"", ..rest] -> {
      Ok(Parsing(string.concat(rest), ""))
    }
    [v, ..rest] -> {
      use next_part <- result.try(do_parse_string(rest))
      Ok(Parsing(remaining: next_part.remaining, value: v <> next_part.value))
    }
    [] -> Error(MissingQuote)
  }
}
