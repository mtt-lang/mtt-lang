open Base
open Result.Let_syntax

type error =
  [ `EnvUnboundRegularVarError of Id.R.t * string  (** variable id, message *)
  | `EnvUnboundModalVarError of Id.M.t * string  (** variable id, message *)
  | `EnvUnboundTypeVarError of Id.T.t * string  (** variable id, message *)
  | `EnvUnboundDCtorVarError of Id.D.t * string  (** variable id, message *) ]

module Make (Key : sig
  type t [@@deriving_inline sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Ppx_sexp_conv_lib.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  include Equal.S with type t := t

  val context_kind : string
  val to_string : t -> string
end) (Error : sig
  val makeError : Key.t * string -> [> error ]
end) =
struct
  type 'v t = (Key.t, 'v) List.Assoc.t [@@deriving sexp]
  (** Environment *)

  (** Empty environment *)
  let emp = []

  (** Extend environment with a key and the corresponding value *)
  let extend env k v = (k, v) :: env

  let narrow env k = List.Assoc.remove env ~equal:Key.equal k

  (** Find the value corresponding to a key identifier
      NOTE: effectively type of error (EnvUnboundRegularVarError or
            EnvUnboundModalVariableError) depends on type of ID, but ocaml can't check it.
   *)
  let lookup env k =
    let typ_o = List.Assoc.find env k ~equal:Key.equal in
    match typ_o with
    | Some t -> return t
    | None ->
        let var_name = Key.to_string k in
        let message =
          [%string
            "\"$(var_name)\" is not found in the $(Key.context_kind) \
             environment!"]
        in
        let error = Error.makeError (k, message) in
        Result.fail error
end

(** Regular environment *)
module R =
  Make
    (Id.R)
    (struct
      let makeError p = `EnvUnboundRegularVarError p
    end)

(** Modal environment *)
module M =
  Make
    (Id.M)
    (struct
      let makeError p = `EnvUnboundModalVarError p
    end)

(** Data constructors environment *)
module D =
  Make
    (Id.D)
    (struct
      let makeError p = `EnvUnboundDCtorVarError p
    end)

(** Types environment *)
module T =
  Make
    (Id.T)
    (struct
      let makeError p = `EnvUnboundTypeVarError p
    end)
