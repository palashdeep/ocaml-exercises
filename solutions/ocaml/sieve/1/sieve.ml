let primes n =
  let rec sieve = function
    | [] -> []
    | p :: xs -> p :: sieve (List.filter (fun x -> x mod p <> 0) xs)
  in
  sieve (List.init (n-1) (fun i -> i+2))
      
        
      
