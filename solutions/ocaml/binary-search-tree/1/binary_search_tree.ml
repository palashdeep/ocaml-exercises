open Base

type bst = 
| Empty
| Node of {
    value : int;
    left : bst;
    right : bst;
}

let empty = Empty

let value = function
| Empty -> Error "Empty tree"
| Node {value; _} -> Ok value

let left = function
| Empty -> Error "Empty tree"
| Node {left; _} -> Ok left

let right = function
| Empty -> Error "Empty tree"
| Node {right; _} -> Ok right

let rec insert x = function
| Empty -> Node {value = x; left = Empty; right = Empty}
| Node {value; left; right} as node ->
  if x <= value then Node {value; left = insert x left; right}
  else Node {value; left; right = insert x right}

let to_list tree = 
  let rec aux acc = function
    | Empty -> acc
    | Node {value; left; right} as node -> aux (value :: aux acc right) left
    in
    aux [] tree
