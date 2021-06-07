let located_to_string Location.{ data = error; loc } =
  match error with
  | `TypeMismatchError msg -> Location.pp ~msg loc
  | `EvaluationError msg -> Location.pp ~msg loc
  | `EnvUnboundRegularVarError (_, msg) -> Location.pp ~msg loc
  | `EnvUnboundModalVarError (_, msg) -> Location.pp ~msg loc
  | `UnboundRegularVarInsideBoxError (_, msg) -> Location.pp ~msg loc

let to_string error =
  match error with
  | `EvaluationError msg -> msg
  | `EnvUnboundRegularVarError (_, msg) -> msg
  | `EnvUnboundModalVarError (_, msg) -> msg
  | `TypeMismatchError msg -> msg
  | `CompilationError msg -> msg
