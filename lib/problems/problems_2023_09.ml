let year = 2023
let day = 9

let parse_input input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun x -> x <> "")
  |> List.map (fun x -> x |> String.split_on_char ' ' |> List.map int_of_string)

let process_next_line line = 
  let rec aux acc l =
    match l with
    | x :: y :: tl -> aux (acc @ [y - x]) (y :: tl)
    | _ :: [] -> acc
    | [] -> raise (Invalid_argument "")
  in
  aux [] line

let rec process acc lines =
  let next = lines |> process_next_line in
  if next |> List.for_all (fun x -> x = 0) then
    acc
  else
    process (next :: acc) next

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let lines = input |> parse_input in
    let result =
      lines
      |> List.map (fun x -> x |> process [x] |> List.map (fun x -> x |> List.rev |> List.hd))
      |> List.concat
      |> List.fold_left (+) 0
    in
    Ok (result |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let lines = input |> parse_input in
    let result =
      lines
      |> List.map (fun x -> x |> process [x] |> List.map List.hd)
      |> List.map (fun x -> x |> List.tl |> List.fold_left (fun acc x -> x - acc) (x |> List.hd))
      |> List.fold_left (+) 0
    in
    Ok (result |> string_of_int)
end
