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
