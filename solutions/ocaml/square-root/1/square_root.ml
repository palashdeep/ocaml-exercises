let square_root n =
  let rec aux x =
    let next = (x + n / x) / 2 in
    if next >= x then x else aux next
  in
  aux n
    