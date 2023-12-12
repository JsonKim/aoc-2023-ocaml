let year = 2023
let day = 5

let string_to_int64 s = s |> Int64.of_string

let is_in source destination range position =
  if ((Int64.compare source position) <= 0) && (Int64.compare position (Int64.add source range)) < 0 then
    Some (Int64.add (Int64.sub position source) destination)
  else
    None

let next_position map position = 
  map
  |> List.fold_left (
    fun acc (destination, source, range) ->
      match acc with
      | None -> is_in source destination range position
      | Some(_) -> acc
  ) None
  |> Option.value ~default: position

module Part_1 = struct
  let parse_seeds input =
    input
    |> String.split_on_char ' '
    |> List.tl
    |> (List.filter (fun x -> (x |> String.trim |> String.length) > 0))
    |> List.map string_to_int64
  
  let parse_map input =
    input
    |> String.split_on_char '\n'
    |> List.tl
    |> List.filter (fun x -> (x |> String.length) > 0)
    |> List.map (String.split_on_char ' ')
    |> List.map (function
        | x :: y :: z :: [] -> (x |> string_to_int64, y |> string_to_int64, z |> string_to_int64)
        | x :: [] -> raise (Invalid_argument (Printf.sprintf "line of map must have three items, but %s." x))
        | l -> raise (Invalid_argument (Printf.sprintf "line of map must have 3 items, but %d." (l |> List.length)))
    )

  let run (input : string) : (string, string) result =
    let spliteed = input |> Str.split (Str.regexp "\n\n") |> (List.filter (fun x -> (x |> String.length) > 0)) in
    let seeds = spliteed |> List.hd |> parse_seeds in
    let maps = spliteed |> List.tl |> List.map parse_map in
    let seeds' = seeds |> List.map (fun seed -> List.fold_left (fun acc map -> next_position map acc) seed maps) in
    let result = seeds' |> List.fold_left Int64.min Int64.max_int in 

    Ok (result |> Int64.to_string)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
