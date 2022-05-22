open Mtt

(* Util functions *)
let doc2string document =
  let buffer = Buffer.create 100 in
  PPrint.ToBuffer.pretty 1.0 80 buffer document;
  Buffer.contents buffer

let eval_repl expr =
  match Util.parse_and_eval expr with
  | Ok res -> Ok (doc2string @@ PrettyPrinter.Doc.of_val res)
  | Error err_msg -> Error err_msg

let infer_repl expr =
  match Util.parse_and_typeinfer expr with
  | Ok res -> Ok (doc2string @@ PrettyPrinter.Doc.of_type res)
  | Error err_msg -> Error err_msg

let parse_repl expr =
  match Util.parse_from_e Term expr with
  | Ok res -> Ok (doc2string @@ PrettyPrinter.Doc.of_expr res)
  | Error err_msg -> Error err_msg

module MttRepl : sig
  type state

  (* initial state *)
  val initial_state : state

  (* final state *)
  val exit_state : state

  (* the greeting message *)
  val greeting : string

  (* repl prompt *)
  val prompt : state -> string

  (* runs command in current state and return pair [new state, result] or error *)
  val eval : state -> string -> (state * string, string) Result.t
end = struct
  type state = { n : int }

  type command =
    | Eval of string
    | Infer of string
    | Print of string
    | Help
    | Logo
    | Exit
  [@@deriving equal, sexp]

  let initial_state = { n = 1 }
  let exit_state = { n = -1 }

  let greeting =
    "Welcome to MTT REPL!\n\
     Enter an expression for evaluation or type `:help` for more information"

  let prompt state = Printf.sprintf "[%d]> " state.n

  let safe_string_index str c =
    try Some (String.index str c) with Not_found -> None

  let lsplit_by_space s =
    match safe_string_index s ' ' with
    | Some space_index ->
        let l = Str.string_before s space_index in
        let r = Str.string_after s (space_index + 1) in
        (l, r)
    | None -> (s, "")

  let parse_instruction instr =
    if String.get instr 0 != ':' then Ok (Eval instr)
    else
      let cmd, expr = lsplit_by_space instr in
      match cmd with
      | ":eval" -> Ok (Eval expr)
      | ":infer" -> Ok (Infer expr)
      | ":print" -> Ok (Print expr)
      | ":help" -> Ok Help
      | ":logo" -> Ok Logo
      | ":exit" -> Ok Exit
      | _ ->
          Error
            (Printf.sprintf "Unknown command `%s`." cmd
            ^ "Please, use the `:help` command to see a list of available \
               commands")

  let help_message =
    "A list of available command:\n\
    \  <term>            evaluate/run <term>\n\
    \  :eval <term>      evaluate/run <term>\n\
    \  :infer <term>     infer the type of a <term>\n\
    \  :print <term>     parse and pretty-print a <term>\n\
    \  :help             print this message\n\
    \  :exit             exit repl"

  let mtt_logo =
    "\n\
    \ __  __ _____ _____   ____  _____ ____  _     \n\
     |  \\/  |_   _|_   _| |  _ \\| ____|  _ \\| |    \n\
     | |\\/| | | |   | |   | |_) |  _| | |_) | |    \n\
     | |  | | | |   | |   |  _ <| |___|  __/| |___ \n\
     |_|  |_| |_|   |_|   |_| \\_\\_____|_|   |_____|\n\
     ==============================================\n\
    \ "

  let exit_message = "Leaving MTT REPL."

  let run_command = function
    | Eval expr -> eval_repl (String expr)
    | Infer expr -> infer_repl (String expr)
    | Print expr -> parse_repl (String expr)
    | Help -> Ok help_message
    | Logo -> Ok mtt_logo
    | Exit -> Ok exit_message

  let eval state instr =
    let open Base.Result.Let_syntax in
    let new_state = { n = state.n + 1 } in
    let%bind cmd = parse_instruction instr in
    Result.map
      (fun res ->
        if res == exit_message then (exit_state, res) else (new_state, res))
      (run_command cmd)
end

open React
open Lwt
open LTerm_text
open LTerm_style

(* Customization of the read-line engine *)
class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = false
    initializer self#set_prompt (S.const (eval [ S (MttRepl.prompt state) ]))
  end

(* Main loop *)
let rec loop term history state =
  if state == MttRepl.exit_state then return ()
  else
    Lwt.catch
      (fun () ->
        let rl =
          new read_line ~term ~history:(LTerm_history.contents history) ~state
        in
        rl#run >|= fun command -> Some command)
      (function Sys.Break -> return None | exn -> Lwt.fail exn)
    >>= function
    | Some command ->
        (let command_utf8 = Zed_string.to_utf8 command in
         match MttRepl.eval state command_utf8 with
         | Ok (new_state, res) ->
             LTerm.fprintls term (eval [ S res ]) >>= fun () -> return new_state
         | Error err_msg ->
             LTerm.fprintls term (eval [ B_fg red; S err_msg; E_fg ])
             >>= fun () -> return state)
        >>= fun next_state ->
        LTerm_history.add history command;
        loop term history next_state
    | None -> loop term history state

(* Entry point *)
let main () =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch
    (fun () ->
      let state = MttRepl.initial_state in
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.printls (eval [ B_fg green; S MttRepl.greeting; E_fg ])
      >>= fun () -> loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)

let () = Lwt_main.run (main ())
