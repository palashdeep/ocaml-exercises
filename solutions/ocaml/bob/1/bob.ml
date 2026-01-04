let is_alpha c =
  let i = Char.code (Char.lowercase_ascii c) in
  i >= Char.code 'a' && i <= Char.code 'z'

let is_alpha_str s =
  String.exists is_alpha s

let response_for str = 
let s = String.trim str in
let len = String.length s in
let has_letter = is_alpha_str s in
let is_yelling = has_letter && String.uppercase_ascii s = s in
let is_ques = len > 0 && s.[len-1] = '?' in
if len = 0 then "Fine. Be that way!"
else if is_ques && is_yelling then "Calm down, I know what I'm doing!"
else if is_ques then "Sure."
else if is_yelling then "Whoa, chill out!"
else "Whatever."