let update arr c =
  if Char.Ascii.is_letter c then
    let i = Char.code (Char.lowercase_ascii c) - Char.code 'a' in
    arr.(i) <- arr.(i) + 1

let is_isogram (word: string) : bool =
  let alpha = Array.make 26 0 in
  String.iter (fun c -> update alpha c) word;
  not (Array.exists (fun i -> i > 1) alpha)