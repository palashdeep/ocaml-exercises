let score (x: float) (y: float): int =
  let func x y = x**2. +. y**2. in
  match func x y with
  | v when v <= 1. -> 10
  | v when v <= 25. -> 5
  | v when v <= 100. -> 1
  | _ -> 0
