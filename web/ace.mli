open Js_of_ocaml
module Html = Dom_html

type editor

val create_editor : string -> editor

val clear_selection : editor -> unit

val get_value : editor -> string

val set_mode : editor -> string -> unit

val set_options : editor -> string -> unit

val set_theme : editor -> string -> unit

val set_value : editor -> string -> unit
