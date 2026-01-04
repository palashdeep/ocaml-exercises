let tuple_to_map (a, b) = 
  let rec aux acc ys = 
    match ys with
    | [] -> acc
    | x :: xs -> aux ((Char.lowercase_ascii x, a)::acc) xs
  in
  aux [] b

let transform lst =
  List.sort compare (List.concat_map tuple_to_map lst)