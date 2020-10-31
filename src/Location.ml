open Base

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving equal, sexp]

type t = { start_position : position; end_position : position }
[@@deriving equal, sexp]

type 'a location = { data : 'a; loc : t } [@@deriving equal, sexp]

let mkPosition start_position end_position = { start_position; end_position }

let mkLocByPosition data start_position end_position =
  let loc = mkPosition start_position end_position in
  { data; loc }

let mkLocation data location =
  let loc = mkPosition location.start_position location.end_position in
  { data; loc }

let showPos pos =
  let file =
    if String.length pos.pos_fname <> 0 then
      String.concat [ "file name : "; pos.pos_fname ]
    else String.concat [ "file name : "; "Not a file" ]
  in
  let line =
    String.concat
      [ "line number : "; Sexp.to_string ([%sexp_of: int] pos.pos_lnum) ]
  in
  let offs =
    String.concat [ "offset : "; Sexp.to_string ([%sexp_of: int] pos.pos_bol) ]
  in
  String.concat ~sep:"\n" [ "Show Position: \n"; file; line; offs ]

let showLocation location =
  let show_s =
    String.concat
      [ "start_position : \n"; showPos location.loc.start_position; "----\n" ]
  in
  let show_e =
    String.concat
      [ "end_position : \n"; showPos location.loc.end_position; "----\n" ]
  in
  String.concat ~sep:"\n" [ "Show Location: \n"; show_s; show_e ]

let errorMsg msg location =
  String.concat ~sep:"\n" [ msg; showLocation location ]
