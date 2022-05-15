open Js_of_ocaml
module Html = Dom_html

class type ['a] ace_editor =
  object
    method clearSelection : unit Js.meth
    method getValue : Js.js_string Js.t Js.meth
    method setMode : Js.js_string Js.t -> unit Js.meth
    method setOptions : Js.json Js.t -> unit Js.meth
    method setTheme : Js.js_string Js.t -> unit Js.meth
    method setValue : Js.js_string Js.t -> unit Js.meth
  end

type editor = { editor : editor ace_editor Js.t }

let create_editor div =
  let editor =
    Js.Unsafe.new_obj
      (Js.Unsafe.pure_js_expr "ace.edit")
      [| Js.Unsafe.inject (Js.string div) |]
  in
  let data = { editor } in
  data

let clear_selection { editor } = editor##clearSelection
let get_value { editor } = Js.to_string editor##getValue

let set_mode { editor } mode =
  let session = Js.Unsafe.meth_call editor "getSession" [||] in
  session ## (setMode (Js.string mode))

let set_theme { editor } theme = editor ## (setTheme (Js.string theme))
let set_value { editor } value = editor ## (setValue (Js.string value))

let set_options { editor } options =
  let json_options = Json.unsafe_input @@ Js.string options in
  editor ## (setOptions json_options)
