open Elpi
open Ast


let rec type_pp fmt = function
  | Type.Unit -> Format.fprintf fmt "unit"
  | Type.Prod { ty1; ty2 } -> Format.fprintf fmt "Prod %a %a" type_pp ty1 type_pp ty2
  | Type.Arr { dom; cod } -> Format.fprintf fmt "%a -> %a" type_pp dom type_pp cod
  | _ -> Format.fprintf fmt "unsupported"

let type_conversion = let open API.AlgebraicData in declare {
  ty = API.Conversion.TyName "ty";
  doc = "MTT type";
  pp = type_pp;
  constructors = [
   K("unit", "type with one element",
     N, (* ty *)
     B Type.Unit,
     M (fun ~ok ~ko -> function Type.Unit -> ok | _ -> ko ()));
   K("product", "Cartesian product of two types",
     S(S(N)), (* ty -> ty -> ty *)
     B (fun ty1 ty2 -> Type.Prod {ty1; ty2 }),
     M (fun ~ok ~ko -> function Type.Prod { ty1; ty2 } -> ok ty1 ty2 | _ -> ko ()));
   K("arr", "function type",
     S(S(N)), (* ty -> ty -> ty *)
     B (fun dom cod -> Type.Arr { dom; cod }),
     M (fun ~ok ~ko -> function Type.Arr { dom; cod } -> ok dom cod | _ -> ko ()));
   K("base", "opaque type with name tag",
     A(API.BuiltInData.string,N), (* strings -> ty *)
     B (fun idt -> Type.Base { idt }),
     M (fun ~ok ~ko -> function Type.Base { idt } -> ok idt | _ -> ko ()));
  ]
}

let builtins = API.BuiltIn.declare ~file_name:"mtt.elpi" [
  API.BuiltIn.MLDataC type_conversion;
]

let get_outcome = function
  | API.Execute.Success
      { API.Data.assignments; constraints = _; state; pp_ctx = _; _ } ->
        let term = API.Data.StrMap.find "T" assignments in
        let (_, ty, _) = (API.ContextualConversion.(!<) type_conversion).readback ~depth:0 state term in
        ty
  | API.Execute.Failure -> failwith "Failure@\n%!"
  | API.Execute.NoMoreSteps -> failwith "Interrupted (no more steps)@\n%!"

let linfer expr =
  let elpi, _ =
    (API.Setup.init ~builtins:[ Builtin.std_builtins; builtins ] ~basedir:"./core/elpi")
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
  Ok(get_outcome b)
