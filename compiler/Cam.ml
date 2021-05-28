(* module rec Instructions : sig
  type t
end = struct
  type t =
    | Fst
    | Snd
    | Push
    | Swap
    | Cons
    | App
    | Quote of { n : Values.t }
    | Cur of { prog : t list }
    | Plus
  [@@deriving sexp]
end

and Values : sig
  type t
end = struct
  type t =
    | VUnit
    | VClos of { e : t; p : Instructions.t list }
    | VPair of { e : t; f : t }
    | VNum of { n : int } (* there is no nat in article *)
  [@@deriving sexp] 
end *)

type instructionCAM =
  | IFst
  | ISnd
  | IPush
  | ISwap
  | ICons
  | IApp
  | IQuote of { v : valueCAM }
  | ICur of { prog : instructionCAM list }
  | IPlus

and valueCAM =
  | VUnit
  | VClos of { e : valueCAM; p : instructionCAM list }
  | VPair of { e : valueCAM; f : valueCAM }
  | VNum of { n : int }

(* let rec dump_instruction (inst : instructionCAM) : string =
  match inst with
  | IFst -> "Fst"
  | ISnd -> "Snd"
  | IPush -> "Push"
  | ISwap -> "Swap"
  | ICons -> "Cons"
  | IApp -> "App"
  | IQuote { expr = _ } -> failwith "not implemented"
  (* | Num { n } -> "Num " ^ Nat.to_string n *)
  | ICur { prog } -> "Cur " ^ "[" ^ dump_instructions prog ^ "]"
  | IPlus -> "Plus"

and dump_instructions (program : instructionCAM list) : string =
  let strings = List.map dump_instruction program in
  String.concat ";\n" strings

let rec dump_value (value : valuesCAM) =
  match value with
  | VUnit -> "VUnit"
  | VClos { e; p } ->
      "VClos[ arg=" ^ dump_value e ^ "; intrs = {" ^ dump_instructions p ^ "}]"
  | VPair { e; f } -> "VPair{ " ^ dump_value e ^ " ; " ^ dump_value f ^ "}"
  | VNum { n } -> "VNum {" ^ Int.to_string n ^ "}" *)

let cam2val v =
  let open Mtt.Ast in
  match v with
  | VUnit -> Val.Unit
  | VNum { n } -> Val.Nat { n = Mtt.Nat.of_int n }
  | _ -> failwith "this kind of value isn't implemented"
