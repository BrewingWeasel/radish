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
