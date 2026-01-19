let invalid_punct c =
  not (Char.Ascii.is_digit c)
  && not (Char.Ascii.is_letter c)
  && c <> ' ' && c <> '-' && c <> '(' && c <> ')' && c <> '.' && c <> '+'

let aux = function
| [a;b;c;d;e;f;g;h;i;j] as digits -> if a = '0' then Error "area code cannot start with zero"
                           else if a = '1' then Error "area code cannot start with one"
                           else if d = '0' then Error "exchange code cannot start with zero"
                           else if d = '1' then Error "exchange code cannot start with one"
                           else Ok (String.of_seq (List.to_seq digits))
| _ -> Error "invalid number"

let number n =
  if String.exists Char.Ascii.is_letter n then Error "letters not permitted"
  else if String.exists invalid_punct n then Error "punctuations not permitted"
  else (let num =
  n
  |> String.to_seq
  |> Seq.filter (fun c -> Char.Ascii.is_digit c)
  |> List.of_seq
  in
  let len = List.length num in
  if len > 11 then Error "must not be greater than 11 digits"
  else if len < 10 then Error "must not be fewer than 10 digits"
  else (
  match num with
  | a :: rest when len = 11 -> if a <> '1' then Error "11 digits must start with 1"
                               else aux rest
  | _ -> aux num
  )
  )