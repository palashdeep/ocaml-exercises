let egg_count number =  
  let rec aux n sum =
  if n = 0 then sum
  else aux (n lsr 1) (n land 1) + sum in
  aux number 0