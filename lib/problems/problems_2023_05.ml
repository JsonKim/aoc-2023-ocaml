let year = 2023
let day = 5

let string_to_int s = s |> Int64.of_string |> Int64.to_int

let is_in source destination range position =
  if (source <= position) && (position < source + range) then
    Some ((position - source) + destination)
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
    |> List.map string_to_int
  
  let parse_map input =
    input
    |> String.split_on_char '\n'
    |> List.tl
    |> List.filter (fun x -> (x |> String.length) > 0)
    |> List.map (String.split_on_char ' ')
    |> List.map (function
        | x :: y :: z :: [] -> (x |> string_to_int, y |> string_to_int, z |> string_to_int)
        | x :: [] -> raise (Invalid_argument (Printf.sprintf "line of map must have three items, but %s." x))
        | l -> raise (Invalid_argument (Printf.sprintf "line of map must have 3 items, but %d." (l |> List.length)))
    )

  let run (input : string) : (string, string) result =
    let spliteed = input |> Str.split (Str.regexp "\n\n") |> (List.filter (fun x -> (x |> String.length) > 0)) in
    let seeds = spliteed |> List.hd |> parse_seeds in
    let maps = spliteed |> List.tl |> List.map parse_map in
    let seeds' = seeds |> List.map (fun seed -> List.fold_left (fun acc map -> next_position map acc) seed maps) in
    let result = seeds' |> List.fold_left Int.min Int.max_int in 

    Ok (result |> Int64.of_int |> Int64.to_string)
end

module Part_2 = struct
  let parse_seeds input =
    input
    |> String.split_on_char ' '
    |> List.tl
    |> (List.filter (fun x -> (x |> String.trim |> String.length) > 0))
    |> List.map string_to_int
    |> fun xs ->
        let rec aux acc l =
          match l with
          | a :: b :: tl -> [(a, b)] @ aux acc tl
          | [] -> acc
          | _ -> raise (Invalid_argument "")
        in
          aux [] xs
  
  let parse_map input =
    input
    |> String.split_on_char '\n'
    |> List.tl
    |> List.filter (fun x -> (x |> String.length) > 0)
    |> List.map (String.split_on_char ' ')
    |> List.map (function
        | x :: y :: z :: [] -> (x |> string_to_int, y |> string_to_int, z |> string_to_int)
        | x :: [] -> raise (Invalid_argument (Printf.sprintf "line of map must have three items, but %s." x))
        | l -> raise (Invalid_argument (Printf.sprintf "line of map must have 3 items, but %d." (l |> List.length)))
    )
    |> List.sort (fun (_, a, _) (_, z, _) -> a - z)
    |> List.fold_left (fun acc (destination, source, range) ->
        if (acc |> List.length) = 0 then
          [(destination, source, range)]
        else
          let (_, last_source, last_range) = acc |> List.rev |> List.hd in
          let mid_source = last_source + last_range in
          if (source > mid_source) then
            acc @ [(mid_source, mid_source, source - mid_source); (destination, source, range)]
          else
            acc @ [(destination, source, range)]
      ) []

  let next_positions (map : (int * int * int) list) (seeds : (int * int) list) : (int * int) list =
    let split seed =
      let result = map |> List.fold_left (
        fun ((seed_start, seed_range), acc) (destination, start, range) ->
          if seed_range > 0 && start <= seed_start && seed_start < (start + range) then (
            let seed_range' = min seed_range (range - (seed_start - start)) in
            ((start + range, seed_range - seed_range'), acc @ [(destination + (seed_start - start), seed_range')])
          )else (
            ((seed_start, seed_range), acc)
          )
        ) ((seed), []) in
      let acc = result |> snd in
      let (seed_start, seed_range) = result |> fst in
      if (seed_range = 0) then
        acc
      else
        acc @ [(seed_start, seed_range)]
    in
    seeds |> List.map split |> List.concat

  let run (input : string) : (string, string) result =
    let spliteed = input |> Str.split (Str.regexp "\n\n") |> (List.filter (fun x -> (x |> String.length) > 0)) in
    let seeds = spliteed |> List.hd |> parse_seeds in
    let maps = spliteed |> List.tl |> List.map parse_map in
    let nx = maps |> List.fold_left (fun seeds' map -> next_positions map seeds') seeds in
    let _ = Printf.printf "\n" in
    let _ = nx |> List.iter (fun (a, b) -> Printf.printf "%d, %d\n" a b) in
    let result = nx |> List.map fst |> List.fold_left Int.min Int.max_int in

    Ok (result |> Int64.of_int |> Int64.to_string)
end
