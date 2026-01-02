let raindrop n =
  let sounds =
    (if n mod 3 = 0 then "Pling" else "")^
    (if n mod 5 = 0 then "Plang" else "")^
    (if n mod 7 = 0 then "Plong" else "")
  in
  if String.equal sounds "" then string_of_int n else sounds
  
    
