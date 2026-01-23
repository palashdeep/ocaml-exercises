type base = int

let rec pow a b =
  match b with
  | 0 -> 1
  | _ -> a * (pow a (b-1))

let base_to_ten base digits =
  let len = List.length digits in
  digits
  |> List.mapi (fun i x -> (pow base (len-i-1))*x)
  |> List.fold_left (+) 0

let ten_to_base base number =
  let rec func num acc = 
  if num = 0 then acc
  else func (num/base) ((num mod base)::acc)
  in
  if number = 0 then [0] else func number []
  

let convert_bases ~from ~digits ~target =
    if from <= 1 || target <= 1 || List.exists (fun x -> x < 0 || x >= from) digits then None
    else Some (let num = base_to_ten from digits in
    ten_to_base target num)