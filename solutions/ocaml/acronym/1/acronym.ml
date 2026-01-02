open Base

let acronym s =
  s
  |> String.split ~on:' '
  |> List.concat_map ~f:(String.split ~on:'-')
  |> List.filter_map ~f:(fun w ->
      String.find w ~f:(Char.is_alpha)
      |> Option.map ~f:(Char.uppercase)
  )
  |> String.of_char_list