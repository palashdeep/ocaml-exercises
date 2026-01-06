let is_triangle a b c =
  (a > 0 && b > 0 && c > 0 && (a + b) >= c && (b + c) >= a && (a + c) >= b) 

let is_equilateral a b c =
    let triangle = is_triangle a b c in
    (triangle && a = b && b = c)

let is_isosceles a b c =
    let triangle = is_triangle a b c in
    (triangle && (a = b || b = c || a = c))

let is_scalene a b c =
    let triangle = is_triangle a b c in
    (triangle && not (a = b || b = c || a = c))