(* open Base *)

let located_to_string Location.{ data = error; loc } =
  match error with
  | `TypeMismatchError msg -> Location.pprint ~msg loc
  | `EvaluationError msg -> Location.pprint ~msg loc
  | `EnvUnboundRegularVarError (_, msg) -> Location.pprint ~msg loc
  | `EnvUnboundModalVarError (_, msg) -> Location.pprint ~msg loc
  | `UnboundRegularVarInsideBoxError (_, msg) -> Location.pprint ~msg loc

let to_string error =
  match error with
  | `EvaluationError msg -> msg
  | `EnvUnboundRegularVarError (_, msg) -> msg
  | `EnvUnboundModalVarError (_, msg) -> msg
  | `TypeMismatchError msg -> msg
