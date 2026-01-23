let matching a b =
  match a, b with
  | '{', '}' | '[', ']' | '(', ')' -> true
  | _ -> false
  
let are_balanced str =
  let len = String.length str in
    let rec aux stack i =
      if i = len then
        stack = []
      else
        match str.[i] with
        | '{' | '[' | '(' as c -> aux (c :: stack) (i+1)
        | '}' | ']' | ')' as c -> 
          (match stack with
          | b :: rest when matching b c -> aux rest (i+1)
          | _ -> false)
        | _ -> aux stack (i+1)
        in
        aux [] 0
      