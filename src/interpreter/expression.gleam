import gleam/io
import gleam/bool
import parser.{BracketList, Call, Number, StrVal, UnquotedStr, Variable}
import interpreter.{
  type RanExpression, type RuntimeError, type State, type Value, RadishBool,
  RadishInt, RadishList, RadishStr, RanExpression, Void,
}
import gleam/int
import gleam/list
import gleam/result
import gleam/option.{type Option, None, Some}
import shellout
import gleam/dict

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
    Variable(v) ->
      RanExpression(
        returned: state.variables
          |> dict.get(v)
          |> result.replace_error(interpreter.NonexistentVariable),
        with: state,
      )
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
    "set" -> {
      use #(var_name, value), state <- interpreter.try(case args {
        [UnquotedStr(variable_name), ast] -> {
          use val, state <- interpreter.try(run_expression(state, ast))
          RanExpression(Ok(#(variable_name, val)), state)
        }
        _ -> RanExpression(Error(interpreter.IncorrectType), state)
      })
      RanExpression(
        Ok(Void),
        interpreter.State(
          ..state,
          variables: dict.insert(state.variables, var_name, value),
        ),
      )
    }
    "+" -> apply_int_func_to_args(state, args, int.add)
    "-" -> apply_int_func_to_args(state, args, int.subtract)
    "*" -> apply_int_func_to_args(state, args, int.multiply)
    "==" ->
      generate_bool_from_args(state, args, fn(v1, v2) { v1 == v2 }, do_gen_bool)
    "!=" ->
      generate_bool_from_args(state, args, fn(v1, v2) { v1 != v2 }, do_gen_bool)
    ">" ->
      generate_bool_from_args(state, args, fn(v1, v2) { v1 > v2 }, do_gen_bool)
    "<" ->
      generate_bool_from_args(state, args, fn(v1, v2) { v1 < v2 }, do_gen_bool)
    ">=" ->
      generate_bool_from_args(state, args, fn(v1, v2) { v1 >= v2 }, do_gen_bool)
    "<=" ->
      generate_bool_from_args(state, args, fn(v1, v2) { v1 <= v2 }, do_gen_bool)
    "if" -> {
      case args {
        [condition, parser.BracketList(to_run)] ->
          if_expression(state, condition, to_run, None)
        _ -> RanExpression(Error(interpreter.ExpectedValue), state)
      }
    }
    "if-else" -> {
      case args {
        [
          condition,
          parser.BracketList(when_true),
          parser.BracketList(when_false),
        ] -> if_expression(state, condition, when_true, Some(when_false))
        _ -> RanExpression(Error(interpreter.ExpectedValue), state)
      }
    }
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
    RadishBool(b) -> Ok([bool.to_string(b)])
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

pub fn generate_bool_from_args(
  state: State,
  args: List(parser.Ast),
  condition: fn(a, a) -> Bool,
  handle_windows: fn(List(List(Value)), fn(a, a) -> Bool) ->
    Result(Bool, RuntimeError),
) -> RanExpression(Value) {
  use arg_values, state <- interpreter.try(
    args
    |> interpreter.map(state, run_expression),
  )

  case list.window(arg_values, 2) {
    [] -> RanExpression(returned: Error(interpreter.IncorrectType), with: state)
    v ->
      RanExpression(
        returned: result.map(handle_windows(v, condition), RadishBool),
        with: state,
      )
  }
}

/// generic function that only works if all types are supported
fn do_gen_bool(
  windows: List(List(Value)),
  predicate,
) -> Result(Bool, RuntimeError) {
  case windows {
    [[value1, value2], ..rest] -> {
      let confirm = fn(v1: a, v2: a) {
        case predicate(v1, v2) {
          False -> Ok(False)
          True -> {
            use next <- result.try(do_gen_bool(rest, predicate))
            Ok(next)
          }
        }
      }

      case value1, value2 {
        RadishStr(v1), RadishStr(v2) -> confirm(v1, v2)
        RadishList(v1), RadishList(v2) -> confirm(v1, v2)
        RadishBool(v1), RadishBool(v2) -> confirm(v1, v2)
        RadishInt(v1), RadishInt(v2) -> confirm(v1, v2)
        Void, _ -> Error(interpreter.IncorrectType)
        // fill out the case statement completely so future types aren't forgotten about 
        RadishStr(_), _ -> Error(interpreter.IncorrectType)
        RadishBool(_), _ -> Error(interpreter.IncorrectType)
        RadishInt(_), _ -> Error(interpreter.IncorrectType)
        RadishList(_), _ -> Error(interpreter.IncorrectType)
      }
    }
    _ -> {
      Ok(True)
    }
  }
}

fn do_gen_bool_int(
  windows: List(List(Value)),
  predicate: fn(Int, Int) -> Bool,
) -> Result(Bool, RuntimeError) {
  case windows {
    [[value1, value2], ..rest] -> {
      let confirm = fn(v1, v2) {
        case predicate(v1, v2) {
          False -> Ok(False)
          True -> {
            use next <- result.try(do_gen_bool_int(rest, predicate))
            Ok(next)
          }
        }
      }

      case value1, value2 {
        RadishInt(v1), RadishInt(v2) -> confirm(v1, v2)
        _, _ -> Error(interpreter.IncorrectType)
      }
    }
    _ -> {
      Ok(True)
    }
  }
}

fn if_expression(
  state: State,
  condition: parser.Ast,
  when_true: List(parser.Ast),
  when_false: Option(List(parser.Ast)),
) -> RanExpression(Value) {
  use condition_result, state <- interpreter.try(run_expression(
    state,
    condition,
  ))
  case condition_result {
    RadishBool(True) -> {
      use responses, state <- interpreter.try(interpreter.map(
        when_true,
        state,
        run_expression,
      ))
      case option.is_some(when_false) {
        True ->
          RanExpression(
            responses
              |> list.last()
              |> result.replace_error(interpreter.IncorrectType),
            state,
          )
        False -> RanExpression(Ok(Void), state)
      }
    }
    RadishBool(False) -> {
      case when_false {
        Some(to_run) -> {
          use responses, state <- interpreter.try(interpreter.map(
            to_run,
            state,
            run_expression,
          ))
          RanExpression(
            responses
              |> list.last()
              |> result.replace_error(interpreter.IncorrectType),
            state,
          )
        }
        None -> RanExpression(Ok(Void), state)
      }
    }
    _ -> RanExpression(Error(interpreter.IncorrectType), state)
  }
}
