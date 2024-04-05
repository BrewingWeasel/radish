import interpreter/state
import parser
import gleam/list
import gleam/result

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

pub type RanExpression(t) {
  RanExpression(returned: Result(t, RuntimeError), with: state.State)
}

pub fn map(
  args: List(parser.Ast),
  state: state.State,
  func: fn(state.State, parser.Ast) -> RanExpression(t),
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

pub fn try(
  value: RanExpression(a),
  handle: fn(a, state.State) -> RanExpression(b),
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
