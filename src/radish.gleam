import gleam/io
import gleam/string
import gleam/result
import gleam/erlang
import gleam/list
import shellout

// TODO: try_map throughout

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
  |> result.map(fn(p) { run_expression(p.value) })
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
    Ok(#(c, rest)) if c == " " || c == "\n" || c == "\t" ->
      Ok(Parsing(rest, ""))
    Ok(#(c, _)) if c == ")" || c == "]" -> Ok(Parsing(input, ""))
    Ok(#(c, rest)) -> {
      use parsed_atom <- result.try(parse_atom(rest))
      Ok(Parsing(parsed_atom.remaining, c <> parsed_atom.value))
    }
    Error(Nil) -> Error(UnexpectedFileEnd)
  }
}

pub fn parse_list(
  input: String,
  ending: String,
) -> Result(Parsing(List(Ast)), ParseError) {
  use parsed_list <- result.try(do_parse_list(input, ending))
  Ok(Parsing(parsed_list.remaining, parsed_list.value))
}

fn do_parse_list(
  input: String,
  ending: String,
) -> Result(Parsing(List(Ast)), ParseError) {
  case string.pop_grapheme(string.trim_left(input)) {
    Ok(#(v, rest)) if v == ending -> Ok(Parsing(rest, []))
    Ok(_) -> {
      use cur_elem <- result.try(parse_expression(input))
      use next_items <- result.try(do_parse_list(cur_elem.remaining, ending))
      Ok(Parsing(next_items.remaining, [cur_elem.value, ..next_items.value]))
    }
    Error(_) -> Error(MissingListEnd(ending))
  }
}

pub fn parse_string(input: String) -> Result(Parsing(Ast), ParseError) {
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

pub type RuntimeError {
  InvalidSyntax(SyntaxError)
  ExpectedValue
}

pub type SyntaxError {
  InvalidFuncToCall
  NoFuncToCall
}

pub type Value {
  RadishStr(String)
  RadishList(List(Value))
  Void
}

fn run_expression(expression: Ast) -> Result(Value, RuntimeError) {
  case expression {
    Call([UnquotedStr(func), ..args]) -> call_func(func, args)
    Call([_, ..]) -> Error(InvalidSyntax(InvalidFuncToCall))
    Call([]) -> Error(InvalidSyntax(NoFuncToCall))
    StrVal(s) | UnquotedStr(s) -> Ok(RadishStr(s))
    BracketList(l) ->
      l
      |> list.map(run_expression)
      |> result.all()
      |> result.map(RadishList)
    _ -> todo
  }
}

fn get_string_from_value(value: Value) -> Result(List(String), RuntimeError) {
  case value {
    RadishStr(s) -> Ok([s])
    RadishList(v) -> {
      v
      |> list.map(get_string_from_value)
      |> result.all()
      |> result.map(list.concat)
    }
    Void -> Error(ExpectedValue)
  }
}

fn call_func(func: String, args: List(Ast)) -> Result(Value, RuntimeError) {
  case func {
    custom -> {
      // use arg_values <-  result.try(result.all(list.map(args, run_expression)))
      use arg_values <- result.try(
        args
        |> list.map(run_expression)
        |> result.all(),
      )

      use arg_strings <- result.try(
        arg_values
        |> list.map(get_string_from_value)
        |> result.all(),
      )

      case
        shellout.command(
          run: func,
          with: list.concat(arg_strings),
          in: ".",
          opt: [],
        )
      {
        Ok(s) -> io.println(s)
        Error(e) -> {
          io.debug(e)
          Nil
        }
      }
      Ok(Void)
    }
  }
}
