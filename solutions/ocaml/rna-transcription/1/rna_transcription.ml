type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna dna =
  dna
  |> List.fold_left (fun acc x ->
    match x with
    | `A -> `U :: acc
    | `T -> `A :: acc
    | `C -> `G :: acc
    | `G -> `C :: acc
  ) []
  |> List.rev
