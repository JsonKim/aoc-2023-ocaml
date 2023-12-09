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

module Part_2 = struct
  let run (input: string) : (string, string) result = Ok input
end
