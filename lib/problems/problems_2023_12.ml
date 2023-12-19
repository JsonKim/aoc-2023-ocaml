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
  gears |> List.map (token_to_string) |> String.concat ""

let print_list list = list |> List.iter (fun x -> Printf.printf "%d " x); Printf.printf "\n"

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

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
