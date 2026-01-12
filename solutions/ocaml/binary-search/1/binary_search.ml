let find candidates target =
    let len = Array.length candidates in
    let rec aux start last =
      let mid = (start+last)/2 in
      let arr_mid = candidates.(mid) in
      if start > last then Error "value not in array"
      else if arr_mid = target then Ok mid
      else if arr_mid > target then aux start (mid-1)
      else aux (mid+1) last
      in
      if len > 0 then aux 0 (len-1) else Error "value not in array"