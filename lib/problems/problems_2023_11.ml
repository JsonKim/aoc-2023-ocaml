let year = 2023
let day = 11

let transpose matrix =
  let rec transpose_column = function
    | [] | [] :: _ -> []
    | matrix -> (List.map List.hd matrix) :: transpose_column (List.map List.tl matrix)
  in
  match matrix with
  | [] -> []
  | _ -> transpose_column matrix

module Part_1 = struct
  let expansion_space (space : char list list) =
    List.fold_right (fun x acc ->
      let acc' = if x |> List.for_all (fun x -> x = '.') then x :: acc else acc in
      x :: acc'
    ) space []

  let distance (x1, y1) (x2, y2) =
    (abs (x2 - x1)) + (abs (y2 - y1))

  let run (input : string) : (string, string) result =
    let rec go acc l =
      match l with
      | p1 :: tl -> tl |> List.fold_left (fun acc p2 -> acc + (distance p1 p2)) (go acc tl)
      | [] -> acc
    in

    let space = input |> String.split_on_char '\n' |> List.map (fun x -> x |> String.to_seq |> List.of_seq) |> List.filter (fun x -> x != []) in
    let expanded_space = space |> expansion_space |> transpose |> expansion_space |> transpose in
    let collected_galaxies =
      expanded_space
      |> List.fold_left (fun (x, acc) line -> 
          let (_, acc') = line |> List.fold_left (fun (y, acc) ch -> if (ch = '.') then (y + 1, acc) else (y + 1, acc @ [(x, y)])) (0, []) in
          (x + 1, acc @ acc')) (0, [])
      |> snd
    in
    let result = collected_galaxies |> go 0 in
    Ok (result |> string_of_int)
end

module Part_2 = struct
  let expansion_space (space : char list list) =
    List.fold_right (fun x acc ->
      let x' = if x |> List.for_all (fun c -> c = '.' || c = '+') then x |> List.map (fun _ -> '+') else x in
      x' :: acc
    ) space []

  let distance space deep (x1, y1) (x2, y2) =
    let xs = if x1 = x2 then [] else (List.init (abs (x2 - x1)) (fun n -> n + (min x1 x2) + 1)) in
    let ys = if y1 = y2 then [] else (List.init (y2 - y1) (fun n -> n + y1 + 1)) in
    let xs' = xs |> List.map (fun x -> List.nth (List.nth space y1) x) in
    let ys' = ys |> List.map (fun y -> List.nth (List.nth space y) x2) in
    (xs' @ ys')
    |> List.fold_left (fun acc c -> acc + (if c = '+' then deep else 1)) 0

  let rec go space deep acc l =
    match l with
    | p1 :: tl -> tl |> List.fold_left (fun acc p2 -> acc + (distance space deep p1 p2)) (go space deep acc tl)
    | [] -> acc

  let run (input : string) : (string, string) result = 
    let space = input |> String.split_on_char '\n' |> List.map (fun x -> x |> String.to_seq |> List.of_seq) |> List.filter (fun x -> x != []) in
    let expanded_space = space |> expansion_space |> transpose |> expansion_space |> transpose in
    let collected_galaxies =
      expanded_space
      |> List.fold_left (fun (y, acc) line -> 
          let (_, acc') = line |> List.fold_left (fun (x, acc) ch -> if (ch != '#') then (x + 1, acc) else (x + 1, acc @ [(x, y)])) (0, []) in
          (y + 1, acc @ acc')) (0, [])
      |> snd
    in
    let result = collected_galaxies |> go expanded_space 1000000 0 in
    Ok (result |> string_of_int)
end
