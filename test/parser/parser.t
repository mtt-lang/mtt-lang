Parse both ASCII and Unicode keywords for projections
  $ mtt parse -e "λx: (A * B) * (C * D). <π₁ (fst x), π₂ (snd x)>"
  λx : A×B×(C×D). <π₁(π₁x),
  π₂(π₂x)>

Pretty-printing for term-level box value
  $ mtt parse -e "box ()"
  box ()

Parsing and pretty-printing of open terms
  $ mtt parse -e "x"
  x
  $ mtt parse -e "(f x) y"
  (f x) y
