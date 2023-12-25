let year = 2023
let day = 13

let parse_input input =
  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map String.trim
  |> List.filter (fun x -> x <> "")
  |> List.map (String.split_on_char '\n')
  |> List.map (List.map (fun x -> x |> String.to_seq |> List.of_seq))

let get_last l = l |> List.rev |> List.hd

let transpose matrix =
  let rec transpose_column = function
    | [] | [] :: _ -> []
    | matrix -> (List.map List.hd matrix) :: transpose_column (List.map List.tl matrix)
  in
  match matrix with
  | [] -> []
  | _ -> transpose_column matrix

let alt l r =
  match l, r with
  | None, _ -> r
  | l, _ -> l

let ( <|> ) = alt

module Part_1 = struct
  let process pattern =
    let rec aux' cursor p =
      match p with
      | [] -> Some (cursor - 1)
      | [_] -> None
      | hd :: tl ->
          let last = tl |> get_last in
          if (hd = last) then
            aux' (cursor + 1) (tl |> List.rev |> List.tl |> List.rev)
          else
            None
    in

    let rec aux cursor p =
      let cursor' = cursor + 1 in
      match p with
      | [hd; last] when hd = last -> Some cursor
      | [_; _] | [_] -> None
      | hd :: tl -> (
          let last = tl |> get_last in
          if (hd = last) then
            (aux' cursor' (tl |> List.rev |> List.tl |> List.rev)) <|> (aux cursor' tl)
          else
            aux cursor' tl
        )
      | [] -> None
    in aux 1 pattern

  let run (input : string) : (string, string) result =
    let result =
      input
      |> parse_input
      |> List.map (fun pattern -> 
          (pattern |> transpose |> process)
          <|> (pattern |> transpose |> List.rev |> process |> Option.map (fun x -> (pattern |> transpose |> List.length) - x))
          <|> (pattern |> process |> Option.map (fun x -> x * 100))
          <|> (pattern |> List.rev |> process |> Option.map (fun x -> ((pattern |> List.length) - x) * 100))
        )
      |> List.map Option.get
    in
    Ok (result |> List.fold_left (+) 0 |> string_of_int)
end

module Part_2 = struct
  let get_diff_count l r =
    List.combine l r |> List.filter (fun (l, r) -> l <> r) |> List.length

  let rec narrow cursor diff_count p =
    match p with
    | [] when diff_count = 1 -> Some (cursor - 1)
    | [] | [_] -> None
    | hd :: tl ->
        let last = tl |> get_last in
        let diff_count' = diff_count + get_diff_count hd last in
        if (diff_count' <= 1) then
          narrow (cursor + 1) diff_count' (tl |> List.rev |> List.tl |> List.rev)
        else
          None

  let process p =
    let rec aux cursor p =
      let cursor' = cursor + 1 in
      match p with
      | [_] -> None
      | [hd; last] -> (
        if get_diff_count hd last = 1 then (
          Some cursor
        ) else
          None
      )
      | hd :: tl -> (
          let last = tl |> get_last in
          let diff_count' = get_diff_count hd last in
          if (diff_count' <= 1) then
            (narrow cursor' diff_count' (tl |> List.rev |> List.tl |> List.rev)) <|> (aux cursor' tl)
          else
            aux cursor' tl
        )
      | [] -> None
    in aux 1 p

  let run (input : string) : (string, string) result =
    let result =
      input
      |> parse_input
      |> List.map (fun pattern -> 
          (pattern |> transpose |> process)
          <|> (pattern |> transpose |> List.rev |> process |> Option.map (fun x -> (pattern |> transpose |> List.length) - x))
          <|> (pattern |> process |> Option.map (fun x -> x * 100))
          <|> (pattern |> List.rev |> process |> Option.map (fun x -> ((pattern |> List.length) - x) * 100))
        )
      |> List.map Option.get
    in
    Ok (result |> List.fold_left (+) 0 |> string_of_int)
end
