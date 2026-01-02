let score (x: float) (y: float): int =
  let r2 = x*.x +. y*.y in
  if r2 <= 1. then 10
  else if r2 <= 25. then 5
  else if r2 <= 100. then 1
  else 0
