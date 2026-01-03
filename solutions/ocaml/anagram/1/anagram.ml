open Base

let char_to_int c = (Char.to_int (Char.lowercase c)) - (Char.to_int 'a')

let aux str = 
  let arr = Array.create ~len:26 0 in
  String.iter str ~f:(fun c -> 
  if Char.is_alpha c then 
    let i = char_to_int c in
    arr.(i) <- arr.(i) + 1);
  arr

let anagrams target candidates =
  let target_sig = aux target in
  let target_norm = String.lowercase target in
  let target_len = String.length target in
  List.filter candidates ~f:(fun cand ->
    (String.length cand = target_len)
    && String.(lowercase cand <> target_norm)
    && (Array.equal Int.equal target_sig (aux cand))
  )