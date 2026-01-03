type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let mappings = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats]

let mapped = function
| 0 -> Eggs
| 1 -> Peanuts
| 2 -> Shellfish
| 3 -> Strawberries
| 4 -> Tomatoes
| 5 -> Chocolate
| 6 -> Pollen
| _ -> Cats

let allergic_to n all =
  let m = List.find_index (fun x -> x = all) mappings in
  match m with
  | Some v -> if ((1 lsl v) land n) = 0 then false else true
  | None -> false
   
let allergies n =
  let rec func n i acc =
    if i > n || i > 7 then acc
    else if ((1 lsl i) land n) = 0 then 
      func n (i+1) acc
    else func n (i+1) ((mapped i) :: acc)
    in
    List.rev (func n 0 [])
