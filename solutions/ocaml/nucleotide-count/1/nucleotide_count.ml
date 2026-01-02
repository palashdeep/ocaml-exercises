open Base

let empty = Map.empty (module Char)

let count_nucleotide s c =
  match c with
  | 'A' | 'C' | 'T' | 'G' ->
    String.fold_result s ~init:0 ~f:(fun acc x ->
    match x with
    | 'A' | 'T' | 'C' | 'G' -> Ok (acc + if Char.equal x c then 1 else 0)
    | _ -> Error x)
  | _ -> Error c
  
let count_nucleotides s =
  let init = empty
    in
    String.fold_result s ~init ~f:(fun acc c ->
    match c with
    | 'A' | 'T' | 'C' | 'G' -> Ok (Map.update acc c ~f:(function
      | None -> 1
      | Some n -> (n+1)))
    | _ -> Error c
  ) 
