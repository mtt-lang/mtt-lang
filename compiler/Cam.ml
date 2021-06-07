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
      (** 
    This instructions is needed for
    correct substitution in `letbox` expression.
    i is De Bruijn index  *)
  | IVar of { i : int }
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
  | IVar { i } -> "Var [ i=" ^ string_of_int i ^ "]"
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
  | VNum { n } -> "VNum {" ^ string_of_int n ^ "}"

let rec genidx insts generated =
  let rec instr_for_var (n : int) =
    if Base.phys_equal n 0 then [ ISnd ] else IFst :: instr_for_var (n - 1)
  in
  match insts with
  | [] -> generated
  | IVar { i } :: s -> genidx s (generated @ instr_for_var i)
  | ICur { prog } :: s ->
      genidx s (generated @ [ ICur { prog = genidx prog [] } ])
  | ICurRec { prog } :: s ->
      genidx s (generated @ [ ICurRec { prog = genidx prog [] } ])
  | IBranch { cond; c1; c2 } :: s ->
      genidx s
        (generated
        @ [
            IBranch
              { cond = genidx cond []; c1 = genidx c1 []; c2 = genidx c2 [] };
          ])
  | IQuote { v } :: s -> genidx s (generated @ [ IQuote { v = genval v } ])
  | e :: s -> genidx s (generated @ [ e ])

and genval value =
  match value with
  | VClos { e; p } -> VClos { e = genval e; p = genidx p [] }
  | VClosRec { e; p } -> VClosRec { e = genval e; p = genidx p [] }
  | VPair { e; f } -> VPair { e = genval e; f = genval f }
  | v -> v

let rec cam2val v =
  let open Mtt.Ast in
  match v with
  | VUnit -> Val.Unit
  | VNum { n } -> Val.Nat { n = Mtt.Nat.of_int n }
  | VPair { e; f } -> Val.Pair { v1 = cam2val e; v2 = cam2val f }
  | VClos _ -> failwith "VClos isn't implemented"
  | VClosRec _ -> failwith "VClosRec isn't implemented"
