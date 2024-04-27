import gleam/dict.{type Dict}
import gleam/list
import gleam/otp/port.{type Port}
import gleam/result
import parser

pub type RuntimeError {
  InvalidSyntax(SyntaxError)
  ExpectedValue
  IncorrectType
  CommandError
  MissingArgument
  NonexistentVariable
}

pub type SyntaxError {
  InvalidFuncToCall
  NoFuncToCall
  Parsing(parser.ParseError)
}

pub type Value {
  RadishStr(String)
  RadishList(List(Value))
  RadishInt(Int)
  RadishBool(Bool)
  Void
}

pub type State {
  State(
    variables: Dict(String, Value),
    request_port: Port,
    response_port: String,
  )
}

pub fn new_state(request_port: Port, response_port: String) -> State {
  State(dict.new(), request_port, response_port)
}

pub type RanExpression(t) {
  RanExpression(returned: Result(t, RuntimeError), with: State)
}

pub fn map(
  args: List(a),
  state: State,
  func: fn(State, a) -> RanExpression(t),
) -> RanExpression(List(t)) {
  let #(value, state) =
    list.fold(over: args, from: #(Ok([]), state), with: fn(acc, elem) {
      case acc.0 {
        Ok(vals) -> {
          case func(acc.1, elem) {
            RanExpression(returned: Ok(v), with: new_state) -> #(
              Ok([v, ..vals]),
              new_state,
            )
            RanExpression(returned: Error(e), with: new_state) -> #(
              Error(e),
              new_state,
            )
          }
        }
        Error(_) -> acc
      }
    })
  RanExpression(returned: result.map(value, list.reverse), with: state)
}

pub fn try_each(
  args: List(a),
  state: State,
  func: fn(State, a) -> RanExpression(t),
) -> RanExpression(Nil) {
  let v =
    list.try_fold(args, state, fn(state, v) {
      let expression = func(state, v)
      case expression.returned {
        Ok(_) -> Ok(expression.with)
        Error(e) -> Error(#(e, expression.with))
      }
    })
  case v {
    Ok(new_state) -> RanExpression(Ok(Nil), new_state)
    Error(#(e, new_state)) -> RanExpression(Error(e), new_state)
  }
}

pub fn try(
  value: RanExpression(a),
  handle: fn(a, State) -> RanExpression(b),
) -> RanExpression(b) {
  case value {
    RanExpression(returned: Ok(v), with: state) -> handle(v, state)
    RanExpression(returned: Error(e), with: state) ->
      RanExpression(returned: Error(e), with: state)
  }
}

pub fn map_result(
  value: RanExpression(a),
  handle: fn(a) -> b,
) -> RanExpression(b) {
  case value {
    RanExpression(returned: Ok(v), with: state) ->
      RanExpression(returned: Ok(handle(v)), with: state)
    RanExpression(returned: Error(e), with: state) ->
      RanExpression(returned: Error(e), with: state)
  }
}
