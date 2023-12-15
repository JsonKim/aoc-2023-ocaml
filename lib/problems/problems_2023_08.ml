let year = 2023
let day = 8

module NodeMap = Map.Make(struct
  type t = string

  let compare = compare
end)

let parse_line line =
  if Str.string_match (Str.regexp {|^\([0-9A-Z]+\) = (\([0-9A-Z]+\), \([0-9A-Z]+\))$|}) line 0 then
    (Str.matched_group 1 line, Str.matched_group 2 line, Str.matched_group 3 line)
  else
    raise (Invalid_argument "")

let parse_input input = 
  input
  |> String.split_on_char '\n'
  |> (fun x -> (
        x |> List.hd |> String.to_seq |> Queue.of_seq,
        x |> List.tl |> List.filter (fun x -> x <> "") |> List.map parse_line |> List.fold_left (fun acc (n, l, r) -> acc |> NodeMap.add n (l, r)) NodeMap.empty
      ))

let pop_and_push q =
  let x = q |> Queue.pop in
  q |> Queue.push x;
  x

module Part_1 = struct
  let rec go instructions map count node =
    if node = "ZZZ" then
      count
    else
      let instruction = instructions |> pop_and_push in
      let next = map |> NodeMap.find node in
      let next' = if instruction = 'L' then next |> fst else next |> snd in
      go instructions map (count + 1) next'

  let run (input : string) : (string, string) result =
    let (instructions, map) = input |> parse_input in
    let count = go instructions map 0 "AAA" in
    Ok (count |> string_of_int)
end

module Part_2 = struct
  let rec gcd u v =
    if v <> 0 then (gcd v (u mod v))
    else (abs u)

  let lcm m n =
    match m, n with
    | 0, _ | _, 0 -> 0
    | m, n -> abs (m * n) / (gcd m n)

  let rec go instructions map count node =
    if node |> String.ends_with ~suffix:"Z" then
      count
    else
      let instruction = instructions |> pop_and_push in
      let next = map |> NodeMap.find node in
      let next' = if instruction = 'L' then next |> fst else next |> snd in
      go instructions map (count + 1) next'

  let run (input : string) : (string, string) result =
    let (instructions, map) = input |> parse_input in
    let start_nodes =
      map
      |> NodeMap.bindings
      |> List.map fst
      |> List.filter (String.ends_with ~suffix:"A")
    in
    let counts = start_nodes |> List.map (go instructions map 0) in
    let result = counts |> List.fold_left lcm 1 in
    Ok (result |> string_of_int)
end
