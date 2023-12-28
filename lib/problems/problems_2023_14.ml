let year = 2023
let day = 14

let transpose matrix =
  let rec transpose_column = function
    | [] | [] :: _ -> []
    | matrix -> (List.map List.hd matrix) :: transpose_column (List.map List.tl matrix)
  in
  match matrix with
  | [] -> []
  | _ -> transpose_column matrix

let split_list delim l = 
  List.fold_right (fun x acc ->
    match acc with
    | _ when x = delim -> [] :: acc
    | hd :: tl -> (hd @ [x]) :: tl
    | [] -> raise (Invalid_argument "")) l [[]]

let join (separate : 'a) (l : 'a list) =
  let rec aux acc l' =
    match l' with
    | [] -> []
    | hd :: [] -> hd :: acc
    | hd :: tl -> aux (separate :: hd :: acc) tl
  in l |> aux [] |> List.rev

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let cal_line (l : char list list) =
      List.fold_right(fun x (last_cube_pos, acc) ->
        let cube_pos = last_cube_pos + (x |> List.length) in
        let rounded_rocks_count = x |> List.filter (fun r -> r = 'O') |> List.length in
        let load = (cube_pos + (cube_pos - rounded_rocks_count + 1)) * rounded_rocks_count / 2 in
        (cube_pos + 1, acc + load)
      ) l (0, 0)
    in

    let result =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun x -> x <> "")
      |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
      |> transpose
      |> List.map (split_list '#')
      |> List.map (cal_line)
      |> List.map (snd)
      |> List.fold_left (fun acc x -> acc + x) 0
    in
    Ok (result |> string_of_int)
end

module Part_2 = struct
  type direction = Left | Right

  let tilt direction l =
    l
    |> List.map (split_list '#')
    |> List.map (fun x -> x |> List.map (fun x ->
        let (rounded, cube) = x |> List.partition (fun x -> x = 'O') in
        match direction with
        | Left -> (rounded @ cube)
        | Right -> (cube @ rounded)
      ))
    |> List.map (join ['#'])
    |> List.map (List.concat)

  let cycle l = 
    l
    |> transpose |> tilt Left |> transpose
    |> tilt Left
    |> transpose |> tilt Right |> transpose
    |> tilt Right

  let find_index_opt pred l =
    let rec aux acc l =
      match l with 
      | [] -> None
      | hd :: _ when pred hd -> Some acc
      | _ :: tl -> aux (acc + 1) tl
    in aux 0 l

  let find_repeated_cycle l =
    let rec aux acc l' =
      let l'' = cycle l' in
      match acc |> find_index_opt (fun x -> x = l'') with
      | Some x -> (x, acc)
      | None -> aux (acc @ [l'']) l''
    in (aux [] l)

  let run (input : string) : (string, string) result =
    let result =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun x -> x <> "")
      |> List.map (fun x -> x |> String.to_seq |> List.of_seq)
      |> find_repeated_cycle
      |> fun (start, lists) ->
          let count = (lists |> List.length) - start in
          let pos = ((1000000000 - start) mod count) + start - 1 in
          List.nth lists pos 
      |> fun x -> List.fold_right (fun x (line, acc) ->
          let rounded_count = x |> List.filter (fun x' -> x' = 'O') |> List.length in
          (line + 1, acc + (rounded_count * line))
        ) x (1, 0)
    in
    Ok (result |> snd |> string_of_int)
end
