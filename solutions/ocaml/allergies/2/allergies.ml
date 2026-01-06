type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let mappings = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats]

let allergic_to n all =
  match List.find_index (( = ) all) mappings with
  | Some v -> ((1 lsl v) land n) <> 0
  | None -> false
   
let allergies n =
  mappings
  |> List.mapi (fun i all -> (i, all))
  |> List.filter_map (fun (i, all) -> if ((1 lsl i) land n) <> 0 then Some all else None)
