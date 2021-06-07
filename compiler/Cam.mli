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

val cam2val : valueCAM -> Mtt.Ast.Val.t

val dump_instructions : instructionCAM list -> string

val dump_value : valueCAM -> string

val genidx : instructionCAM list -> instructionCAM list -> instructionCAM list

val genval : valueCAM -> valueCAM
