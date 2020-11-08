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

type 'a located = { data : 'a; loc : t [@equal.ignore] } [@@deriving equal, sexp]

let mk s_pos e_pos =
  Source
    {
      filename = s_pos.pos_fname;
      start_line = s_pos.pos_lnum;
      start_column = s_pos.pos_cnum;
      end_line = e_pos.pos_lnum;
      end_column = e_pos.pos_cnum;
    }

let locate_start_end data s_pos e_pos =
  let loc = mk s_pos e_pos in
  { data; loc }

let mk_located ?(loc = NoSource) data = { data; loc }

let showPos pos' =
  match pos' with
  | Source pos ->
      let file =
        if String.length pos.filename <> 0 then
          String.concat [ "file name : "; pos.filename ]
        else String.concat [ "file name : "; "Not a file" ]
      in
      let line =
        String.concat
          [
            "lines : ";
            Sexp.to_string ([%sexp_of: int] pos.start_line);
            "-";
            Sexp.to_string ([%sexp_of: int] pos.end_line);
          ]
      in
      let column =
        String.concat
          [
            "column : ";
            Sexp.to_string ([%sexp_of: int] pos.start_column);
            "-";
            Sexp.to_string ([%sexp_of: int] pos.end_column);
          ]
      in
      String.concat ~sep:", " [ file; line; column ]
  | NoSource -> "No position"

let pp ?(msg = "") loc = String.concat ~sep:"\n" [ msg; showPos loc ]
