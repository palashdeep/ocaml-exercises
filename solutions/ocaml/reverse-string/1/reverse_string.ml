open Base

let reverse_string s =
  let len = String.length s in
  let buf = Bytes.create len in
  for i = 0 to len-1 do
    Bytes.set buf i s.[len-1-i]
  done;
  Bytes.to_string buf
    
