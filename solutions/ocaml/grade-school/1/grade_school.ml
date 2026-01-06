open Base

module Int_map = Map.M(Int)
type school = string list Int_map.t

let empty_school = Map.empty (module Int)

let add name gr sch =
    let students =
    match Map.find sch gr with
    | Some lst -> name::lst
    | None -> [name]
    in
    Map.set sch ~key:gr ~data:students   

let grade gr sch =
    match Map.find sch gr with
    | Some lst -> List.sort ~compare:String.compare lst
    | None -> []

let sorted sch =
   Map.map sch ~f:(List.sort ~compare:String.compare)

let roster sch =
    sch
    |> sorted
    |> Map.to_alist
    |> List.concat_map ~f:snd
