let year = 2023
let day = 1

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let rec calibration cs =
  match cs with
  | [] -> raise (Invalid_argument "empty list is invalid")
  | c :: [] -> [c; c] 
  | c1 :: c2 :: [] -> [c1; c2]
  | c :: _ :: cs' -> calibration (c :: cs')

let rec decode cs =
  match cs with
  | 'o' :: 'n' :: 'e' :: tl               -> '1' :: decode ('e' :: tl)
  | 't' :: 'w' :: 'o' :: tl               -> '2' :: decode ('o' :: tl)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: tl -> '3' :: decode ('e' :: tl)
  | 'f' :: 'o' :: 'u' :: 'r' :: tl        -> '4' :: decode tl
  | 'f' :: 'i' :: 'v' :: 'e' :: tl        -> '5' :: decode ('e' :: tl)
  | 's' :: 'i' :: 'x' :: tl               -> '6' :: decode tl
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: tl -> '7' :: decode ('n' :: tl)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: tl -> '8' :: decode ('t' :: tl)
  | 'n' :: 'i' :: 'n' :: 'e' :: tl        -> '9' :: decode ('e' :: tl)
  | c :: tl when is_digit c               -> c   :: decode tl
  | _ :: tl                               -> decode tl
  | [] -> []

let process_line line = 
  line
  |> String.to_seq |> List.of_seq
  |> List.filter is_digit
  |> calibration
  |> List.to_seq |> String.of_seq
  |> Int64.of_string

let process_line2 line = 
  line
  |> String.to_seq |> List.of_seq
  |> decode
  |> calibration
  |> List.to_seq |> String.of_seq
  |> Int64.of_string


module Part_1 = struct
  let run (input : string) : (string, string) result =
    let lines = String.split_on_char '\n' input in
    Ok (lines |> List.filter (fun l -> (String.length l) > 0) |> List.map process_line |> List.fold_left (Int64.add) Int64.zero |> Int64.to_string)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    let lines = String.split_on_char '\n' input |> List.filter (fun l -> String.length l > 0) in
    let r = lines |> List.map process_line2 in
    (* Ok (r |> List.map Int64.to_string |> String.concat "\n") *)
    Ok (r |> List.fold_left Int64.add Int64.zero |> Int64.to_string)
end
