import gleam/io
import gleam/string
import gleam/result
import gleam/erlang
import gleam/list
import shellout
import gleam/bool
import gleam/int

// TODO: try_map throughout

pub type Ast {
  Call(contents: List(Ast), piped: Bool)
  StrVal(String)
  BracketList(List(Ast))
  UnquotedStr(String)
  Number(Int)
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
    v -> {
      use atom <- result.try(parse_atom(v))
      Ok(
        Parsing(atom.remaining, case int.parse(atom.value) {
          Ok(i) -> Number(i)
          Error(Nil) -> UnquotedStr(atom.value)
        }),
      )
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
  IncorrectType
  CommandError
  MissingArgument
}

pub type SyntaxError {
  InvalidFuncToCall
  NoFuncToCall
}

pub type Value {
  RadishStr(String)
  RadishList(List(Value))
  RadishInt(Int)
  Void
}

fn run_expression(expression: Ast) -> Result(Value, RuntimeError) {
  case expression {
    Call([UnquotedStr(func), ..args], piped) -> call_func(func, args, piped)
    Call([_, ..], _) -> Error(InvalidSyntax(InvalidFuncToCall))
    Call([], _) -> Error(InvalidSyntax(NoFuncToCall))
    StrVal(s) | UnquotedStr(s) -> Ok(RadishStr(s))
    Number(i) -> Ok(RadishInt(i))
    BracketList(l) ->
      l
      |> list.map(run_expression)
      |> result.all()
      |> result.map(RadishList)
  }
}

fn get_string_from_value(value: Value) -> Result(List(String), RuntimeError) {
  case value {
    RadishStr(s) -> Ok([s])
    RadishInt(s) -> Ok([int.to_string(s)])
    RadishList(v) -> {
      v
      |> list.map(get_string_from_value)
      |> result.all()
      |> result.map(list.concat)
    }
    Void -> Error(ExpectedValue)
  }
}

fn get_string_from_args(args: List(Ast)) -> Result(List(String), RuntimeError) {
  use arg_values <- result.try(
    args
    |> list.map(run_expression)
    |> result.all(),
  )

  arg_values
  |> list.map(get_string_from_value)
  |> result.all()
  |> result.map(list.concat)
}

fn call_func(
  func: String,
  args: List(Ast),
  piped: Bool,
) -> Result(Value, RuntimeError) {
  case func {
    "+" -> apply_int_func_to_args(args, int.add)
    _ -> {
      use arg_strings <- result.try(get_string_from_args(args))

      let output =
        shellout.command(run: func, with: arg_strings, in: ".", opt: [])
        |> result.replace_error(CommandError)
      use <- bool.guard(when: piped, return: result.map(output, RadishStr))
      case output {
        Ok(str) -> io.println(str)
        Error(e) -> {
          io.debug(e)
          Nil
        }
      }
      Ok(Void)
    }
  }
}

fn apply_int_func_to_args(args, func) -> Result(Value, RuntimeError) {
  apply_func_to_args(
    args,
    fn(x) {
      case x {
        RadishInt(v) -> Ok(v)
        _ -> Error(IncorrectType)
      }
    },
    func,
    RadishInt,
  )
}

fn apply_func_to_args(
  args: List(Ast),
  handle_type: fn(Value) -> Result(a, RuntimeError),
  func: fn(a, a) -> a,
  final_type: fn(a) -> Value,
) -> Result(Value, RuntimeError) {
  use arg_values <- result.try(
    args
    |> list.map(run_expression)
    |> result.all(),
  )
  arg_values
  |> list.try_map(handle_type)
  |> result.try(fn(args) {
    args
    |> list.reduce(func)
    |> result.replace_error(MissingArgument)
    |> result.map(final_type)
  })
}
