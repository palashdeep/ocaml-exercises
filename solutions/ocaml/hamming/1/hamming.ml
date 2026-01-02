type nucleotide = A | C | G | T

let hamming_distance d1 d2 =
  let rec aux d1 d2 sum = 
    match d1, d2 with
    | [], [] -> Ok sum
    | x :: xs, y :: ys -> aux xs ys (sum + if x=y then 0 else 1)
    | _ -> Error "strands must be of equal length"
  in
  aux d1 d2 0