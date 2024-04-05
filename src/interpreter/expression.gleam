import gleam/io
import gleam/bool
import parser.{BracketList, Call, Number, StrVal, UnquotedStr}
import interpreter.{
  type RuntimeError, type Value, RadishInt, RadishList, RadishStr, Void,
}
import interpreter/state.{type State}
import gleam/int
import gleam/list
import gleam/result
import shellout

pub fn run_expression(
  state: State,
  expression: parser.Ast,
) -> Result(Value, RuntimeError) {
  case expression {
    Call([UnquotedStr(func), ..args], piped) ->
      call_func(state, func, args, piped)
    Call([_, ..], _) ->
      Error(interpreter.InvalidSyntax(interpreter.InvalidFuncToCall))
    Call([], _) -> Error(interpreter.InvalidSyntax(interpreter.NoFuncToCall))
    StrVal(s) | UnquotedStr(s) -> Ok(RadishStr(s))
    Number(i) -> Ok(RadishInt(i))
    BracketList(l) ->
      l
      |> list.map(run_expression(state, _))
      |> result.all()
      |> result.map(RadishList)
  }
}

pub fn call_func(
  state: State,
  func: String,
  args: List(parser.Ast),
  piped: Bool,
) -> Result(Value, RuntimeError) {
  case func {
    "+" -> apply_int_func_to_args(state, args, int.add)
    "-" -> apply_int_func_to_args(state, args, int.subtract)
    "*" -> apply_int_func_to_args(state, args, int.multiply)
    _ -> {
      use arg_strings <- result.try(get_string_from_args(state, args))

      let output =
        shellout.command(run: func, with: arg_strings, in: ".", opt: [])
        |> result.replace_error(interpreter.CommandError)
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

pub fn get_string_from_value(value: Value) -> Result(List(String), RuntimeError) {
  case value {
    RadishStr(s) -> Ok([s])
    RadishInt(s) -> Ok([int.to_string(s)])
    RadishList(v) -> {
      v
      |> list.map(get_string_from_value)
      |> result.all()
      |> result.map(list.concat)
    }
    Void -> Error(interpreter.ExpectedValue)
  }
}

pub fn get_string_from_args(
  state: State,
  args: List(parser.Ast),
) -> Result(List(String), RuntimeError) {
  use arg_values <- result.try(
    args
    |> list.map(run_expression(state, _))
    |> result.all(),
  )

  arg_values
  |> list.map(get_string_from_value)
  |> result.all()
  |> result.map(list.concat)
}

pub fn apply_int_func_to_args(
  state: State,
  args: List(parser.Ast),
  func: fn(Int, Int) -> Int,
) -> Result(Value, RuntimeError) {
  apply_func_to_args(
    state,
    args,
    fn(x) {
      case x {
        RadishInt(v) -> Ok(v)
        _ -> Error(interpreter.IncorrectType)
      }
    },
    func,
    RadishInt,
  )
}

pub fn apply_func_to_args(
  state: State,
  args: List(parser.Ast),
  handle_type: fn(Value) -> Result(a, RuntimeError),
  func: fn(a, a) -> a,
  final_type: fn(a) -> Value,
) -> Result(Value, RuntimeError) {
  use arg_values <- result.try(
    args
    |> list.map(run_expression(state, _))
    |> result.all(),
  )
  arg_values
  |> list.try_map(handle_type)
  |> result.try(fn(args) {
    args
    |> list.reduce(func)
    |> result.replace_error(interpreter.MissingArgument)
    |> result.map(final_type)
  })
}
