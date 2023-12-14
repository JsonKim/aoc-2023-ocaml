let year = 2023
let day = 6

let ( <> ) (a, b) c = (a, b, c)

module Part_1 = struct
  let parse_line line =
    line
    |> String.split_on_char ' '
    |> List.filter (fun x -> (x |> String.length) > 0)
    |> List.tl
    |> List.map (fun x -> x |> Int64.of_string |> Int64.to_int)

  let make_hold_and_rest length =
    List.init length (fun n -> (n, length - n))

  let run (input : string) : (string, string) result =
    let lines = input |> String.split_on_char '\n' in
    let times = List.nth lines 0 |> parse_line in
    let distances = List.nth lines 1 |> parse_line in
    let result =
      List.combine times distances
      |> List.map (fun (time, distance) -> (time |> make_hold_and_rest, distance))
      |> List.map (fun (splitted, distance) -> splitted |> List.map (fun x -> x <> distance))
      |> List.map (List.filter (fun (hold, rest, distance) -> (hold * rest) >= distance))
      |> List.map List.length
      |> List.fold_left ( * ) 1
    in

    Ok (result |> Int64.of_int |> Int64.to_string)
end

type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

let rec from n step =
  Cons (n, fun () -> from (n + step) step)

module Part_2 = struct
  let parse_line line =
    line
    |> String.split_on_char ':'
    |> (fun x -> List.nth x 1)
    |> Str.global_replace (Str.regexp " ") ""
    |> Int64.of_string |> Int64.to_int

  let run (input : string) : (string, string) result = 
    let lines = input |> String.split_on_char '\n' in
    let times = List.nth lines 0 |> parse_line in
    let distance = List.nth lines 1 |> parse_line in
    let gap =
      let rec aux stream = match stream with
      | Cons (hd, _) when hd * (times - hd) >= distance -> hd
      | Cons (_, tl) -> aux (tl ())
      | _ -> 0
      in aux (from 0 1)
    in
    let result = times - (gap * 2) + 1 in

    Ok (result |> Int64.of_int |> Int64.to_string)
end
