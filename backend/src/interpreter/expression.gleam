import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/port.{type Port}
import gleam/result
import gleam/string
import glexec
import interpreter.{
  type RanExpression, type RuntimeError, type State, type Value, RadishBool,
  RadishInt, RadishList, RadishStr, RanExpression, Void,
}
import parser.{BracketList, Call, Number, StrVal, UnquotedStr, Variable}

pub fn run_expression(
  state: State,
  expression: parser.Ast,
) -> RanExpression(Value) {
  case expression {
    Call([UnquotedStr(func), ..args], piped) ->
      call_func(state, func, Parsed(args), piped)
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

fn run_expression_from_val(
  state: State,
  expression: Value,
) -> RanExpression(Value) {
  case expression {
    RadishList([RadishStr(command), ..rest]) ->
      call_func(state, command, Value(rest), False)
    _ -> RanExpression(Ok(expression), state)
  }
}

pub type FunctionArguments {
  Parsed(List(parser.Ast))
  Value(List(Value))
}

pub fn call_func(
  state: State,
  func: String,
  args: FunctionArguments,
  piped: Bool,
) -> RanExpression(Value) {
  case func {
    "set" -> {
      use #(var_name, value), state <- interpreter.try(case args {
        Parsed([UnquotedStr(variable_name), ast]) -> {
          use val, state <- interpreter.try(run_expression(state, ast))
          RanExpression(Ok(#(variable_name, val)), state)
        }
        Value([RadishStr(variable_name), value]) ->
          RanExpression(Ok(#(variable_name, value)), state)
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
    "run" -> {
      use args, state <- interpreter.try(get_values_from_args(state, args))
      case args {
        [RadishList([RadishStr(f), ..rest])] ->
          call_func(state, f, Value(rest), False)
        _ -> RanExpression(Error(interpreter.IncorrectType), state)
      }
    }
    "if" -> {
      case args {
        Parsed([condition, parser.BracketList(to_run)]) -> {
          use condition_result, state <- interpreter.try(run_expression(
            state,
            condition,
          ))
          if_expression(state, condition_result, Parsed(to_run), None)
        }
        Value([condition, RadishList(to_run)]) ->
          if_expression(state, condition, Value(to_run), None)
        _ -> RanExpression(Error(interpreter.ExpectedValue), state)
      }
    }
    "if-else" -> {
      case args {
        Parsed([
          condition,
          parser.BracketList(when_true),
          parser.BracketList(when_false),
        ]) -> {
          use condition_result, state <- interpreter.try(run_expression(
            state,
            condition,
          ))
          if_expression(
            state,
            condition_result,
            Parsed(when_true),
            Some(Parsed(when_false)),
          )
        }
        _ -> RanExpression(Error(interpreter.ExpectedValue), state)
      }
    }
    _ -> {
      use arg_strings, state <- interpreter.try(get_string_from_args(
        state,
        args,
      ))

      use command, state <- interpreter.try(RanExpression(
        func
          |> glexec.find_executable()
          |> result.replace_error(interpreter.CommandError),
        state,
      ))

      // TODO: error handling
      let assert Ok(glexec.Pids(_pid, ospid)) =
        glexec.new()
        |> glexec.with_stdin(glexec.StdinPipe)
        |> glexec.with_stdout(glexec.StdoutCapture)
        |> glexec.with_stderr(glexec.StderrCapture)
        |> glexec.with_monitor(True)
        |> glexec.with_pty(True)
        |> glexec.run_async(glexec.Execve([command, ..arg_strings]))

      let stdin = process.start(fn() { pipe_stdin(ospid, state) }, False)

      pipe_stdout(ospid, state, stdin)
      RanExpression(Ok(Void), state)
    }
  }
}

const timeout = 500_000

fn pipe_stdin(running_command, state: State) {
  let response = read_stdin(state.request_port)
  case response {
    <<_, _, _, _, "end":utf8>> -> {
      let assert Ok(Nil) = glexec.send_eof(running_command)
      Nil
    }
    <<_, _, _, _, v>> -> {
      let assert Ok(Nil) =
        glexec.send(
          running_command,
          string.from_utf_codepoints([
            {
              let assert Ok(u) = string.utf_codepoint(v)
              u
            },
          ]),
        )
      pipe_stdin(running_command, state)
    }
    _ -> Nil
  }
}

@external(erlang, "socket_connections", "read_stdin")
fn read_stdin(socket: Port) -> BitArray

fn pipe_stdout(running_command, state: State, stdin_task) {
  case glexec.obtain(50) {
    Ok(glexec.ObtainStdout(_, v)) | Ok(glexec.ObtainStderr(_, v)) -> {
      send_stdout(state.response_port, v)
      pipe_stdout(running_command, state, stdin_task)
    }
    Error(glexec.ObtainTimeout) ->
      pipe_stdout(running_command, state, stdin_task)
    Error(_) -> {
      process.kill(stdin_task)
      Nil
    }
    // Error(glexec.ObtainDownNormal(_, _)) -> {
  //   Nil
  // }
  }
  // TODO: error handling
}

@external(erlang, "socket_connections", "send_stdout")
fn send_stdout(out_socket: String, contents: String) -> Nil

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
  args: FunctionArguments,
) -> RanExpression(List(String)) {
  use arg_values, state <- interpreter.try(get_values_from_args(state, args))
  RanExpression(
    returned: arg_values
      |> list.map(get_string_from_value)
      |> result.all()
      |> result.map(list.concat),
    with: state,
  )
}

fn apply_int_func_to_args(
  state: State,
  args: FunctionArguments,
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

fn get_values_from_args(
  state: State,
  args: FunctionArguments,
) -> RanExpression(List(Value)) {
  case args {
    Parsed(vals) ->
      vals
      |> interpreter.map(state, run_expression)
    Value(vals) -> RanExpression(Ok(vals), state)
  }
}

pub fn apply_func_to_args(
  state: State,
  args: FunctionArguments,
  handle_type: fn(Value) -> Result(a, RuntimeError),
  func: fn(a, a) -> a,
  final_type: fn(a) -> Value,
) -> RanExpression(Value) {
  use arg_values, state <- interpreter.try(get_values_from_args(state, args))
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
  args: FunctionArguments,
  condition: fn(a, a) -> Bool,
  handle_windows: fn(List(List(Value)), fn(a, a) -> Bool) ->
    Result(Bool, RuntimeError),
) -> RanExpression(Value) {
  use arg_values, state <- interpreter.try(get_values_from_args(state, args))

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
  condition: Value,
  when_true: FunctionArguments,
  when_false: Option(FunctionArguments),
) -> RanExpression(Value) {
  case condition {
    RadishBool(True) -> {
      use responses, state <- interpreter.try(case when_true {
        Parsed(to_run) -> interpreter.map(to_run, state, run_expression)
        Value(to_run) -> interpreter.map(to_run, state, run_expression_from_val)
      })

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
          use responses, state <- interpreter.try(case to_run {
            Parsed(to_run) -> interpreter.map(to_run, state, run_expression)
            Value(to_run) ->
              interpreter.map(to_run, state, run_expression_from_val)
          })
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
