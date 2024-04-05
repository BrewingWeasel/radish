import gleam/io
import gleam/bool
import parser.{BracketList, Call, Number, StrVal, UnquotedStr}
import interpreter.{
  type RanExpression, type RuntimeError, type Value, RadishInt, RadishList,
  RadishStr, RanExpression, Void,
}
import interpreter/state.{type State}
import gleam/int
import gleam/list
import gleam/result
import shellout

pub fn run_expression(
  state: State,
  expression: parser.Ast,
) -> RanExpression(Value) {
  case expression {
    Call([UnquotedStr(func), ..args], piped) ->
      call_func(state, func, args, piped)
    Call([_, ..], _) ->
      RanExpression(
        Error(interpreter.InvalidSyntax(interpreter.InvalidFuncToCall)),
        state,
      )
    Call([], _) ->
      RanExpression(
        Error(interpreter.InvalidSyntax(interpreter.NoFuncToCall)),
        state,
      )
    StrVal(s) | UnquotedStr(s) -> RanExpression(Ok(RadishStr(s)), state)
    Number(i) -> RanExpression(Ok(RadishInt(i)), state)
    BracketList(l) ->
      l
      |> interpreter.map(state, run_expression)
      |> interpreter.map_result(RadishList)
  }
}

pub fn call_func(
  state: State,
  func: String,
  args: List(parser.Ast),
  piped: Bool,
) -> RanExpression(Value) {
  case func {
    "+" -> apply_int_func_to_args(state, args, int.add)
    "-" -> apply_int_func_to_args(state, args, int.subtract)
    "*" -> apply_int_func_to_args(state, args, int.multiply)
    _ -> {
      use arg_strings, state <- interpreter.try(get_string_from_args(
        state,
        args,
      ))

      let output =
        shellout.command(run: func, with: arg_strings, in: ".", opt: [])
        |> result.replace_error(interpreter.CommandError)

      use <- bool.guard(
        when: piped,
        return: RanExpression(result.map(output, RadishStr), state),
      )
      case output {
        Ok(str) -> io.println(str)
        Error(e) -> {
          io.debug(e)
          Nil
        }
      }
      RanExpression(Ok(Void), state)
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
) -> RanExpression(List(String)) {
  use arg_values, state <- interpreter.try(
    args
    |> interpreter.map(state, run_expression),
  )

  RanExpression(
    returned: arg_values
      |> list.map(get_string_from_value)
      |> result.all()
      |> result.map(list.concat),
    with: state,
  )
}

pub fn apply_int_func_to_args(
  state: State,
  args: List(parser.Ast),
  func: fn(Int, Int) -> Int,
) -> RanExpression(Value) {
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
) -> RanExpression(Value) {
  use arg_values, state <- interpreter.try(
    args
    |> interpreter.map(state, run_expression),
  )

  case list.try_map(arg_values, handle_type) {
    Error(e) -> RanExpression(returned: Error(e), with: state)
    Ok(v) ->
      RanExpression(
        returned: v
          |> list.reduce(func)
          |> result.replace_error(interpreter.MissingArgument)
          |> result.map(final_type),
        with: state,
      )
  }
}
