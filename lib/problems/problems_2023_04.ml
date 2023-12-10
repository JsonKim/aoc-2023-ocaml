let year = 2023
let day = 4

let bimap f (x1, x2) = (f x1, f x2)

module IntSet = Set.Make(Int)

let intset_of_list li =
  List.fold_left (fun set elem -> IntSet.add elem set) IntSet.empty li

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let lines = String.split_on_char '\n' input in
    let result = lines
      |> List.filter_map (fun line -> 
        line
        |> Str.split (Str.regexp {|: \| | |})
        |> function
            | _ :: x1 :: x2 :: [] -> Some (x1, x2)
            | _ -> None
      )
      |> List.map (bimap (String.split_on_char ' '))
      |> List.map (bimap (List.filter (fun s -> (s |> String.length) != 0)))
      |> List.map (bimap (List.map (fun x -> x |> Int64.of_string |> Int64.to_int)))
      |> List.map (bimap intset_of_list)
      |> List.map (fun (x1, x2) -> IntSet.inter x1 x2 |> IntSet.cardinal)
      |> List.filter (fun x -> x != 0)
      |> List.map (fun x -> 2. ** ((x -1) |> Float.of_int))
      |> List.fold_left (+.) 0.
      |> Float.to_string
    in
    Ok result
end

let extend_list l extend_length value =
  l @ List.init extend_length (fun _ -> value)

let list_to_string l =
  let rec aux acc l =
    match l with
    | [] -> acc
    | hd :: [] -> acc ^ (hd |> Int64.of_int |> Int64.to_string)
    | hd :: tl -> aux (acc ^ (hd |> Int64.of_int |> Int64.to_string) ^ "; ") tl
  in aux "" l

module Part_2 = struct
  let run (input: string) : (string, string) result =
    let lines = String.split_on_char '\n' input in
    let _ = Printf.printf "\n" in
    let result = lines
      |> List.filter_map (fun line -> 
        line
        |> Str.split (Str.regexp {|: \| | |})
        |> function
            | _ :: x1 :: x2 :: [] -> Some (x1, x2)
            | _ -> None
      )
      |> List.map (bimap (String.split_on_char ' '))
      |> List.map (bimap (List.filter (fun s -> (s |> String.length) != 0)))
      |> List.map (bimap (List.map (fun x -> x |> Int64.of_string |> Int64.to_int)))
      |> List.map (bimap intset_of_list)
      |> List.map (fun (x1, x2) -> IntSet.inter x1 x2 |> IntSet.cardinal)
      |> List.fold_left (fun (copies, total) win_count -> 
          let (copied, remain) = match copies with
          | [] -> (0, [])
          | hd :: tl -> (hd, tl)
          in
          let copies' = extend_list remain (max (win_count - (remain |> List.length)) 0) 0 in
          let copies' = copies' |> List.fold_left (fun (a, b) c -> if (a > 0) then (a-1, b @ [c+1+copied]) else (0, b @ [c] )) (win_count, []) |> snd in
          let total' = total + 1 + copied in
          (copies', total')) ([], 0)
      |> snd
      |> Int64.of_int |> Int64.to_string
      in
    Ok result
end
