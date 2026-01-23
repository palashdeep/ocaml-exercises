let char_to_int c = Char.code c - Char.code '0'

let transform i x =
  let a = char_to_int x in
  if i mod 2 = 0 then a
  else if a = 9 then 9
  else (2*a) mod 9

let valid str =
  let sum = 
  str
  |> String.to_seq
  |> Seq.filter (fun c -> c <> ' ')
  |> List.of_seq
  |> List.rev
  |> List.mapi (fun i x -> transform i x)
  |> List.fold_left (+) 0 
  in
  if sum mod 10 = 0 && String.length (String.trim str) > 1 && (String.for_all (fun c -> Char.Ascii.is_digit c || c = ' ') str) then true else false
