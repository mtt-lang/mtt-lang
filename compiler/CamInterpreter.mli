open Base
open Cam

type error = [ `InterpretationError of string ]

val interept :
  valueCAM list -> instructionCAM list -> (valueCAM, [> error ]) Result.t
(** interprets the given bytecode with start value (usually VUnit)   *)
