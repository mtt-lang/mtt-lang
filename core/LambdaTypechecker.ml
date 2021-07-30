open Elpi
open ParserInterface

(* remove this code duplication *)
let parse_from_e_linfer :
    type a. a ast_kind -> input_kind -> (a, error) Result.t =
 fun ast_kind source ->
  parse_from ast_kind source
  |> Base.Result.map_error ~f:(fun parse_error ->
         [%string "Parse error: $parse_error"])

let rec prolog2str expr =
  match API.RawData.look ~depth:0 expr with
  (* let _ = print_int cons_code in *)
  | API.RawData.Const cons_code -> (
      match cons_code with -475 -> "()" | _ -> string_of_int cons_code)
  | API.RawData.Lam lam -> "(" ^ prolog2str lam ^ ")"
  | API.RawData.UnifVar (fd, xs) ->
      API.FlexibleData.Elpi.show fd
      ^ String.concat " " @@ List.map prolog2str xs
  | _ -> failwith "this situation can't happen"

let parse_type ty = parse_from_e_linfer Type (String ty)

let get_outcome = function
  | API.Execute.Success
      { API.Data.assignments; constraints = _; state = _; pp_ctx = _; _ } ->
      API.Data.StrMap.find "T" assignments
  | API.Execute.Failure -> failwith "Failure@\n%!"
  | API.Execute.NoMoreSteps -> failwith "Interrupted (no more steps)@\n%!"

let linfer expr =
  let elpi, _ =
    (API.Setup.init ~builtins:[ Builtin.std_builtins ] ~basedir:"./core/elpi")
      []
  in
  let ast =
    API.Parse.program ~elpi ~print_accumulated_files:false [ "mtt-infer.elpi" ]
  in
  let prog = API.Compile.program ~elpi [ ast ] in
  let prolog_term = "(" ^ Typechecker2.translate expr ^ ")" in
  let elpi_query = "of " ^ prolog_term ^ " T" in
  (* let _ = print_string elpi_query in *)
  let g = API.Parse.goal (API.Ast.Loc.initial "(-exec)") elpi_query in
  let query = API.Compile.query prog g in
  let exec = API.Compile.optimize query in
  let b = API.Execute.once ~delay_outside_fragment:false exec in
  let res_term = get_outcome b in
  (* let reststr = API.Ast.Loc.show ast in *)
  let ty_str = prolog2str res_term in
  (* let _ = print_string ty_str in *)
  parse_type ty_str
