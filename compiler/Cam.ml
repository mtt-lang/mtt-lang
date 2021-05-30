(* module type CamInner = sig
  type t
  val dump : t -> string
end

module rec Instruction : CamInner = struct
  type t =
    | Fst
    | Snd
    | Push
    | Swap
    | Cons
    | App
    | Quote of { v : Value.t }
    | Cur of { prog : Instruction.t list }
    | CurRec of { prog : Instruction.t list }
    | Branch of {
        cond : Instruction.t list;
        c1 : Instruction.t list;
        c2 : Instruction.t list;
      }
    | Plus
    | Minus
    | Mul
    | Div
  [@@deriving equal, sexp]

  let rec dump inst =
    match inst with
    | Fst -> "Fst"
    | Quote { v } -> "Quote [" ^ dump v ^ " ]"
    | _ -> failwith ""

end

and Value : CamInner = struct
  type t =
    | Unit
    | Clos of { e : Value.t; p : Instruction.t list }
    | ClosRec of { e : Value.t; p : Instruction.t list }
    | Pair of { e : Value.t; f : Value.t }
    | Num of { n : int }
  [@@deriving equal, sexp]

  let rec dump = failwith ""

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
  | ICurRec of { prog : instructionCAM list }
  | IBranch of {
      cond : instructionCAM list;
      c1 : instructionCAM list;
      c2 : instructionCAM list;
    }
  | IPlus
  | IMinus
  | IMul
  | IDiv

and valueCAM =
  | VUnit
  | VClos of { e : valueCAM; p : instructionCAM list }
  | VClosRec of { e : valueCAM; p : instructionCAM list }
  | VPair of { e : valueCAM; f : valueCAM }
  | VNum of { n : int }

let rec dump_instruction (inst : instructionCAM) : string =
  match inst with
  | IFst -> "Fst"
  | ISnd -> "Snd"
  | IPush -> "Push"
  | ISwap -> "Swap"
  | ICons -> "Cons"
  | IApp -> "App"
  | IQuote { v } -> "Quote " ^ "[" ^ dump_value v ^ "]"
  | IBranch { cond; c1; c2 } ->
      "Branch [ cond=" ^ dump_instructions cond ^ "; c1=" ^ dump_instructions c1
      ^ "; c2=" ^ dump_instructions c2 ^ "]"
  | ICurRec { prog } -> "CurRec [" ^ dump_instructions prog ^ "]"
  | ICur { prog } -> "Cur [" ^ dump_instructions prog ^ "]"
  | IPlus -> "Plus"
  | IMinus -> "Minus"
  | IMul -> "Mult"
  | IDiv -> "Div"

and dump_instructions (program : instructionCAM list) : string =
  let strings = List.map dump_instruction program in
  String.concat ";\n" strings

and dump_value (value : valueCAM) =
  match value with
  | VUnit -> "VUnit"
  | VClos { e; p } ->
      "VClos[ env=" ^ dump_value e ^ "; intrs = {" ^ dump_instructions p ^ "}]"
  | VClosRec { e; p } ->
      "VClosRec [ env=" ^ dump_value e ^ "; instrs = {" ^ dump_instructions p
      ^ "}]"
  | VPair { e; f } -> "VPair{ " ^ dump_value e ^ " ; " ^ dump_value f ^ "}"
  | VNum { n } -> "VNum {" ^ Int.to_string n ^ "}"

let rec cam2val v =
  let open Mtt.Ast in
  match v with
  | VUnit -> Val.Unit
  | VNum { n } -> Val.Nat { n = Mtt.Nat.of_int n }
  | VPair { e; f } -> Val.Pair { v1 = cam2val e; v2 = cam2val f }
  | VClos _ -> failwith "VClos isn't implemented"
  | VClosRec _ -> failwith "VClosRec isn't implemented"
