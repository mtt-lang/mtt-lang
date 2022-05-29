open Base
open Lexing

type t =
  | NoSource
  | Source of {
      filename : string;
      start_line : int;
      start_column : int;
      end_line : int;
      end_column : int;
    }
[@@deriving sexp]

type 'a located = { data : 'a; loc : t [@equal.ignore] }
[@@deriving equal, sexp]

let mk s_pos e_pos =
  Source
    {
      filename = s_pos.pos_fname;
      start_line = s_pos.pos_lnum;
      start_column = s_pos.pos_cnum - s_pos.pos_bol;
      end_line = e_pos.pos_lnum;
      end_column = e_pos.pos_cnum - e_pos.pos_bol;
    }

let locate_start_end data s_pos e_pos =
  let loc = mk s_pos e_pos in
  { data; loc }

let locate ?(loc = NoSource) data = { data; loc }
let empty pos = match pos with NoSource -> true | Source _ -> false

let pprint_column_range pos =
  match pos with
  | Source pos ->
      [%string
        "$(Int.to_string pos.start_column):$(Int.to_string  pos.end_column)"]
  | NoSource -> ""

let show_pos pos' =
  match pos' with
  | Source pos ->
      let file =
        if String.length pos.filename <> 0 then
          [%string "file name :  $(pos.filename)"]
        else "file name :  Not a file"
      in
      let line =
        [%string
          "lines :  $(Int.to_string pos.start_line) - $(Int.to_string \
           pos.end_line)"]
      in
      let column =
        [%string
          "column :  $(Int.to_string pos.start_column) - $(Int.to_string \
           pos.end_column)"]
      in
      [%string "$(file), $(line), $(column)"]
  | NoSource -> "No position"

let pprint ?(msg = "") loc = [%string "$(msg)\n$(show_pos loc)"]
