open Elpi

let print_solution time = function
  | API.Execute.NoMoreSteps -> Format.eprintf "Interrupted (no more steps)@\n%!"
  | API.Execute.Failure -> Format.eprintf "Failure@\n%!"
  | API.Execute.Success { API.Data.assignments; constraints; state; pp_ctx; _ }
    ->
      Format.eprintf "@\nSuccess:@\n%!";
      API.Data.StrMap.iter
        (fun name v ->
          Format.eprintf "  @[<hov 1>%s = %a@]@\n%!" name (API.Pp.term pp_ctx) v)
        assignments;
      Format.eprintf "@\nTime: %5.3f@\n%!" time;
      Format.eprintf "@\nConstraints:@\n%a@\n%!"
        (API.Pp.constraints pp_ctx)
        constraints;
      Format.eprintf "@\nState:@\n%a@\n%!" API.Pp.state state

let linfer _expr =
  let elpi, _ = (API.Setup.init ~builtins:[] ~basedir:".") [] in
  let ast =
    API.Parse.program ~elpi ~print_accumulated_files:false [ "tmp.elpi" ]
  in
  let prog = API.Compile.program ~elpi [ ast ] in
  let g =
    API.Parse.goal
      (API.Ast.Loc.initial "(-exec)")
      (Printf.sprintf "%s." "whd (fun (x\\x)) T")
  in
  let query = API.Compile.query prog g in
  let exec = API.Compile.optimize query in
  let b = API.Execute.once ~delay_outside_fragment:false exec in
  print_solution 1. b
