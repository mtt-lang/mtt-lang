open Mtt

module Instructions = struct
  type t =
    | Fst
    | Snd
    | Push
    | Swap
    | Cons
    | App
    (* Think about quote Quote of Ast.Val.t *)
    | Num of { n : Nat.t } (* Num <=> Quote now *)
    | Cur of { prog : t list }
    | Plus
  [@@deriving sexp]
end

module Values = struct
  type t =
    | VUnit
    | VClos of { e : t; p : Instructions.t list }
    | VPair of { e : t; f : t }
    | VNum of { n : int } (* there is no nat in article *)
  [@@deriving sexp]
end

let rec dump_instruction (inst : Instructions.t) : string =
  match inst with
  | Fst -> "Fst"
  | Snd -> "Snd"
  | Push -> "Push"
  | Swap -> "Swap"
  | Cons -> "Cons"
  | App -> "App"
  | Num { n } -> "Num " ^ Nat.to_string n
  | Cur { prog } -> "Cur " ^ "[" ^ dump_instructions prog ^ "]"
  | Plus -> "Plus"

and dump_instructions (program : Instructions.t list) : string =
  let strings = List.map dump_instruction program in
  String.concat ";\n" strings

let rec dump_value (value : Values.t) =
  match value with
  | VUnit -> "VUnit"
  | VClos { e; p } ->
      "VClos[ arg=" ^ dump_value e ^ "; intrs = {" ^ dump_instructions p ^ "}]"
  | VPair { e; f } -> "VPair{ " ^ dump_value e ^ " ; " ^ dump_value f ^ "}"
  | VNum { n } -> "VNum {" ^ Int.to_string n ^ "}"
