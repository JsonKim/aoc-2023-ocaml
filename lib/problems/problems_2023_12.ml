let year = 2023
let day = 12

type token = Sharp | Dot

let token_to_string = function
| Sharp -> "#"
| Dot -> "."

let parse_ch = function
| '.' -> Some Dot
| '#' -> Some Sharp
| '?' -> None
| _ -> raise (Invalid_argument "")

let parse_line line =
  line |> String.to_seq |> List.of_seq
  |> List.map parse_ch

let rec count_damaged acc gears = match gears with
  | Dot :: Dot :: _ -> count_damaged acc (gears |> List.tl)
  | Dot :: [] -> count_damaged acc (gears |> List.tl)
  | Dot :: Sharp :: _ -> count_damaged (0 :: acc) (gears |> List.tl)
  | Sharp :: Dot :: _ -> count_damaged (((acc |> List.hd) + 1) :: (acc |> List.tl)) (gears |> List.tl)
  | Sharp :: Sharp :: _ -> count_damaged (((acc |> List.hd) + 1) :: (acc |> List.tl)) (gears |> List.tl)
  | Sharp :: [] -> count_damaged (((acc |> List.hd) + 1) :: (acc |> List.tl)) (gears |> List.tl)
  | [] -> acc |> List.rev |> List.filter (fun x -> x > 0)

let gears_to_string gears =
  gears |> List.map (fun x -> x |> Option.map(token_to_string) |> Option.value ~default:"?") |> String.concat ""

let print_list list = list |> List.iter (fun x -> Printf.printf "%d " x); Printf.printf "\n"

let list_to_string list = list |> List.map string_of_int |> fun x -> "[" ^ (String.concat "; " x) ^ "]"

module Part_1 = struct
  let process gears =
    let add acc token = acc |> List.map (fun x -> x @ [token]) in
    let rec aux acc = function
    | None :: tl -> (aux (add acc Sharp) tl) @ (aux (add acc Dot) tl)
    | Some token :: tl -> aux (add acc token) tl
    | [] -> acc
    in aux [[]] gears

  let run (input : string) : (string, string) result = 
    let result =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun x -> (x |> String.equal "") = false)
      |> List.map (fun x -> x |> String.split_on_char ' ')
      |> List.rev_map (fun x -> match x with
          | gears :: counts :: [] -> (
              let c = counts |> String.split_on_char ',' |> List.map int_of_string in
              let g = gears |> parse_line |> process |> List.rev_map (count_damaged [0]) |> List.filter (fun x -> x = c) |> List.length in
              g
            )
          | _ -> raise (Invalid_argument ""))
      |> List.fold_left (+) 0
    in
    Ok (result |> string_of_int)
end

module ProcessState = struct
  type t = {
    counting: int;
    completed_group: int list;
    last: token;
    duplicate: int;
  }

  let empty = {
    counting = 0;
    completed_group = [];
    last = Dot;
    duplicate = 1;
  }
  
  let equal a b = a.counting = b.counting && a.completed_group = b.completed_group

  let show x
    = (x.last |> token_to_string)
    ^ " :: "
    ^ (x.counting |> string_of_int)
    ^ ", "
    ^ (x.completed_group |> list_to_string)
    ^ ", "
    ^ (x.duplicate |> string_of_int)
end

let rec take n list =
  if n > 0 then
    match list with
    | [] -> failwith "Not enough elements in list"
    | x :: xs -> x :: (take (n - 1) xs)
  else []

let rec drop n h =
   if n == 0 then h
   else (drop (n-1) h |> List.tl)

module Part_2 = struct
  let rec all_combinations n arr =
    match n with
    | 0 -> [[]]
    | _ -> List.flatten (List.map (fun x ->
              List.map (fun y -> x :: y) (all_combinations (n - 1) arr))
            arr)

  let normalize counts acc =
    let acc' = acc
    |> List.filter (fun (x : ProcessState.t) ->
        let completed_group_length = x.completed_group |> List.length in
        if ((completed_group_length) > (counts |> List.length)) then (
          false
        )
        else if (x.counting > 0 && (completed_group_length) = (counts |> List.length)) then(
          false
        )
        else if (x.counting = 0) && (x.completed_group <> (counts |> take completed_group_length)) then (
          false
        )
        else if (x.counting > (List.nth_opt counts completed_group_length |> Option.value ~default: 0)) then (
          false
        ) else
          true
      )
    in
      acc'

  let process counts gears =
   let add token (acc : ProcessState.t list) =
      let aux (state : ProcessState.t ) =
        let state' = match state, token with
        | { counting = 0; _ }, Dot -> state
        | { counting; completed_group; _ }, Dot -> { state with counting = 0; completed_group = (completed_group @ [counting]) }
        | { counting; _ }, Sharp -> { state with counting = counting + 1 }
        in
        { state' with last = token; }
      in
      let acc' = acc |> List.map aux |> normalize counts in
      let acc'' = (
      List.fold_right (fun x acc ->
        let (xs, ys) = acc |> List.partition (ProcessState.equal x) in
        if (xs |> List.length) > 0 then (
          (xs |> List.map (fun (x' : ProcessState.t) -> { x' with duplicate = x.duplicate + x'.duplicate })) @ ys
        ) else
          x :: acc) acc' []
      ) in
      acc''
      in
    let rec aux acc l =
      let acc' = (match l with
      | None :: tl -> (aux ((add Sharp acc) @ (add Dot acc)) tl)
      | Some Sharp :: tl -> aux (add Sharp acc) tl
      | Some Dot :: tl -> aux (add Dot acc) tl
      | [] -> acc |> (add Dot)
      ) in
      acc'
    in
    let acc = aux [ProcessState.empty] gears in
    acc

  let x5 separate l =
    let rec insert_sep sep lst acc = match lst with
    | [] -> acc
    | [x] -> x :: acc
    | x :: xs -> insert_sep sep xs (sep @ x :: acc)
    in
    List.init 5 (fun _ -> l)
    |> fun l -> List.rev (insert_sep [separate] (List.rev l) [])
    |> List.concat

  let run (input : string) : (string, string) result =
    let result =
      input
      |> String.split_on_char '\n'
      |> List.filter (fun x -> (x |> String.equal "") = false)
      |> List.map (fun x -> x |> String.split_on_char ' ')
      |> List.rev_map (fun x -> match x with
          | springs :: counts :: [] -> (
              let counts' = counts |> String.split_on_char ',' |> List.map int_of_string |> x5 [] in
              let springs' = springs |> parse_line |> x5 [None] in
              let processed_springs = springs' |> process counts' in
              let x' = processed_springs in
              let x =  x' |> normalize counts' |> List.filter (fun (x : ProcessState.t) -> x.completed_group = counts') in
              x
              |> List.map (fun (x : ProcessState.t) -> x.duplicate)
              |> List.fold_left (+) 0
            )
          | _ -> raise (Invalid_argument ""))
      |> List.fold_left (+) 0
    in
    Ok (result |> string_of_int)
end
