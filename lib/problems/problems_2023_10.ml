let year = 2023
let day = 10

type direction = East | West | South | North | Stop

let next_position width position to_ =
  match to_ with
  | East -> position + 1
  | West -> position - 1
  | South -> position + width
  | North -> position - width
  | Stop -> position

let next_direction go next_pipe =
  let next_direction = match go, next_pipe with
    | East, 'J' -> Some North
    | East, '-' -> Some East 
    | East, '7' -> Some South
    | West, 'L' -> Some North
    | West, '-' -> Some West
    | West, 'F' -> Some South
    | South, '|' -> Some South
    | South, 'L' -> Some East
    | South, 'J' -> Some West
    | North, '|' -> Some North
    | North, 'F' -> Some East
    | North, '7' -> Some West
    | _, 'S' -> Some Stop
    | _ -> None
  in
  next_direction

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    let width = (String.index input '\n') + 1 in
    let next (pos, direction) =
      let next_pos = next_position width pos direction in
      let next_direction' = (next_direction direction input.[next_pos]) |> Option.get in
      next_pos, next_direction'
    in
    let rec go step cur =
      let cur_pos = cur |> List.hd |> fst in
      if cur |> List.for_all (fun (pos, _) -> pos = cur_pos) then
        step
      else
        go (step + 1) (cur |> List.map next)
    in
    let start_position = String.index input 'S' in
    let result =
      [East; West; South; North]
      |> List.map (next_position width start_position)
      |> List.map (fun x -> x, input.[x])
      |> List.combine [East; West; South; North]
      |> List.map (fun (a, (b, c)) -> (a, b, c))
      |> List.map (fun (go, position, pipe) -> next_direction go pipe |> Option.map (fun x -> (position, x)))
      |> List.filter_map (fun x -> x)
      |> go 1
    in
    Ok (result |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let width = (String.index input '\n') + 1 in

    let raycast l = 
      let rec aux count band l =
        match l with
        | pos :: tl ->
          let ch = input.[pos] in
          let ch = if ch = 'S' then '7' else ch in
          (match ch with
          | '.' | '-' | 'S' -> aux count band tl
          | 'L' | 'F' -> aux count (Some ch) tl
          | 'J' when band = (Some 'L') -> aux count None tl
          | '7' when band = (Some 'F') -> aux count None tl
          | 'J' | '7' -> aux (count + 1) None tl
          | '|' -> aux (count + 1) band tl
          | _ -> raise (Invalid_argument (String.make 1 ch)))
        | _ -> count
      in
      let count = (aux 0 None l) in
      count mod 2 = 1
    in

    let next (pos, direction) =
      let next_pos = next_position width pos direction in
      let next_direction' = (next_direction direction input.[next_pos]) |> Option.get in
      next_pos, next_direction'
    in

    let rec go acc cur =
      let next_pos = cur |> next in
      let acc' = (cur |> fst) :: acc in
      if (next_pos |> snd) = Stop then
        acc'
      else
        go acc' (next_pos)
    in

    let rec collect_line line acc l =
      let start = (line - 1) * width in
      let end_ = start + width - 1 in
      match l with
      | hd :: tl when hd < start -> collect_line line acc tl
      | hd :: tl when hd < end_ -> collect_line line (acc @ [hd]) tl
      | _ -> (acc, l)
    in

    let collect_lines l =
      let rec aux line acc rest =
        let (acc', rest') = collect_line line [] rest in
        if (rest' |> List.length) = 0 then
          acc @ [acc']
        else
          aux (line + 1) (acc @ [acc']) rest'
      in
      aux 1 [] l
    in

    let rec check dot_count line =
      match line with
      | x :: y :: tl when (y - x) = 1 -> check dot_count (y :: tl)
      | x :: y :: tl -> (
        let candidate = if raycast (y :: tl) then
          List.init (y - x - 1) (fun n -> x + n + 1)
        else
          []
        in
        check (dot_count + (candidate |> List.length)) (y :: tl)
      )
      | _ :: [] -> check dot_count []
      | [] -> dot_count
    in

    let start_position = String.index input 'S' in
    let result =
      [East; West; South; North]
      |> List.map (next_position width start_position)
      |> List.map (fun x -> x, input.[x])
      |> List.combine [East; West; South; North]
      |> List.map (fun (a, (b, c)) -> (a, b, c))
      |> List.map (fun (go, position, pipe) -> next_direction go pipe |> Option.map (fun x -> (position, x)))
      |> List.filter_map (fun x -> x) |> List.hd
      |> go [start_position]
      |> List.sort Int.compare
      |> collect_lines
      |> List.fold_left (fun dot_count line -> check dot_count line) 0
    in
    Ok (result |> string_of_int)
end
