let collatz_conjecture input =
  if input > 0 then Ok (
  let rec loop n steps =
    if n = 1 then steps
    else if n mod 2 = 0 then loop (n/2) (steps+1)
    else loop (3*n+1) (steps+1)
    in
    loop input 0
  )
  else Error "Only positive integers are allowed"