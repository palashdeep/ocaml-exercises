let char_to_digit = function
  | 'X' -> 10
  | c -> Char.code c - Char.code '0'

let is_valid isbn =
  let chars =
  isbn
  |> String.to_seq
  |> Seq.filter (fun c -> c <> '-')
  |> List.of_seq
  in
  match chars with
  | [a;b;c;d;e;f;g;h;i;j] -> 
    let digits = 
    [a;b;c;d;e;f;g;h;i]
    |> List.for_all Char.Ascii.is_digit
    && Char.Ascii.is_digit j || j = 'X'
    in
    if not digits then false
    else
      let sum = List.mapi (fun idx c -> (char_to_digit c)*(10-idx)) chars
      |> List.fold_left (+) 0
      in
      sum mod 11 = 0
  | _ -> false