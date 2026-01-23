let int_sqrt n =
  n
  |> float_of_int
  |> sqrt
  |> int_of_float

let classify num = 
  if num > 1 then
  (let sq = int_sqrt num in
  let rec aux acc i =
    if i > sq then acc
    else if num mod i = 0 then
    (if i <> num/i then aux ([i; num/i] @ acc) (i+1)
    else aux (i :: acc) (i+1))
    else aux acc (i+1)
    in
    let sum = List.fold_left (+) 0 (aux [1] 2) in
    if num = sum then Ok "perfect"
    else if num > sum then Ok "deficient"
    else Ok "abundant"
  )
  else if num = 1 then Ok "deficient"
  else Error "Classification is only possible for positive integers."