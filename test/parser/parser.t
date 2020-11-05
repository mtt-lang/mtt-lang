Parse both ASCII and Unicode keywords for projections
  $ mtt parse -e "λx: (A * B) * (C * D). <π₁ (fst x), π₂ (snd x)>"
  (λx:A×B×(C×D). <(π₁(π₁x)),
  (π₂(π₂x))>)
