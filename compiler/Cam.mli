type instructionCAM =
  | IFst
  | ISnd
  | IPush
  | ISwap
  | ICons
  | IApp
  | IQuote of { v : valueCAM }
  | ICur of { prog : instructionCAM list }
  | IBranch of {
      cond : instructionCAM list;
      c1 : instructionCAM list;
      c2 : instructionCAM list;
    }
  | IPlus
  | IMinus

and valueCAM =
  | VUnit
  | VClos of { e : valueCAM; p : instructionCAM list }
  | VPair of { e : valueCAM; f : valueCAM }
  | VNum of { n : int }

val cam2val : valueCAM -> Mtt.Ast.Val.t

val dump_instructions : instructionCAM list -> string

val dump_value : valueCAM -> string
