let year = 2023
let day = 7

module CardMap = Map.Make(struct
  type t = char

  let compare = compare
end)

module Part_1 = struct
  let card_to_score str =
    let add = function
    | None -> Some(1)
    | Some(x) -> Some(x + 1) in

    str
    |> String.to_seq |> List.of_seq
    |> List.fold_left (fun acc c ->
        acc
        |> CardMap.update c add
      ) CardMap.empty
    |> CardMap.to_seq |> List.of_seq
    |> List.map (fun (_, v) -> v)
    |> List.sort (Int.compare)
    |> List.rev
    |> function
        | [5] -> "6"
        | [4; 1] -> "5"
        | [3; 2] -> "4"
        | [3; 1; 1] -> "3"
        | [2; 2; 1] -> "2"
        | [2; 1; 1; 1] -> "1"
        | [1; 1; 1; 1; 1] -> "0"
        | _ -> raise (Invalid_argument "")

    let card_to_hex card =
      card
      |> String.to_seq |> List.of_seq
      |> List.map (function
          | 'A' -> 'E'
          | 'K' -> 'D'
          | 'Q' -> 'C'
          | 'J' -> 'B'
          | 'T' -> 'A'
          | c -> c
      )
      |> List.to_seq |> String.of_seq

  let run (input : string) : (string, string) result =
    let result =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun s -> s <> "")
      |> List.map (fun s -> s |> String.split_on_char ' ')
      |> List.map (function
          | [x; y] -> (x, y)
          | _ -> raise (Invalid_argument "!")
        )
      |> List.map (fun (card, score) -> ("0x" ^ card_to_score card ^ card_to_hex card) |> int_of_string, score |> int_of_string)
      |> List.sort (fun (a, _) (z, _) -> Int.compare a z)
      |> List.fold_left (fun (acc, i) (_, score) -> (acc + (score * i), i + 1)) (0, 1)
      |> fst
      |> string_of_int
    in
    Ok result
end

module Part_2 = struct
  let card_to_score str =
    let add = function
    | None -> Some(1)
    | Some(x) -> Some(x + 1) in

    str
    |> String.to_seq |> List.of_seq
    |> List.fold_left (fun acc c ->
        acc
        |> CardMap.update c add
      ) CardMap.empty
    |> fun x -> (x |> CardMap.find_opt 'J', x |> CardMap.remove 'J')
    |> fun (j, cards) -> (
        match j with
        | None -> cards
        | Some(n) -> (let (key_of_max, _) = CardMap.fold (fun key value (k, length) -> ((if (length > value) then k else key), max length value)) cards (' ', 0) in
        cards |> CardMap.update key_of_max (Option.map (fun x -> x + n)))
      )
    |> CardMap.bindings
    |> List.map (fun (_, v) -> v)
    |> List.sort (Int.compare)
    |> List.rev
    |> function
        | [] | [5] -> "6"
        | [4; 1] -> "5"
        | [3; 2] -> "4"
        | [3; 1; 1] -> "3"
        | [2; 2; 1] -> "2"
        | [2; 1; 1; 1] -> "1"
        | [1; 1; 1; 1; 1] -> "0"
        | _ -> raise (Invalid_argument "")

    let card_to_hex card =
      card
      |> String.to_seq |> List.of_seq
      |> List.map (function
          | 'A' -> 'E'
          | 'K' -> 'D'
          | 'Q' -> 'C'
          | 'T' -> 'A'
          | 'J' -> '1'
          | c -> c
      )
      |> List.to_seq |> String.of_seq

  let run (input : string) : (string, string) result =
    let result =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun s -> s <> "")
      |> List.map (fun s -> s |> String.split_on_char ' ')
      |> List.map (function
          | [x; y] -> (x, y)
          | _ -> raise (Invalid_argument "!")
        )
      |> List.map (fun (card, score) -> ("0x" ^ card_to_score card ^ card_to_hex card) |> int_of_string, score |> int_of_string)
      |> List.sort (fun (a, _) (z, _) -> Int.compare a z)
      |> List.fold_left (fun (acc, i) (_, score) -> (acc + (score * i), i + 1)) (0, 1)
      |> fst
      |> string_of_int
    in
    Ok result
end
