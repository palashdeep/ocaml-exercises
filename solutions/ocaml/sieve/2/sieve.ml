let primes n =
  let prime = Array.make (n+1) true in
  prime.(0) <- false;
  prime.(1) <- false;

  for i = 2 to int_of_float (sqrt(float n)) do
    if prime.(i) then
      for j = i*i to n do
        if j mod i = 0 then prime.(j) <- false
      done
  done;

  let result = ref [] in
    for i = n downto 2 do
      if prime.(i) then result:= i :: !result
    done;
  !result
  
        
      
