let year = 2023
let day = 3

module Token = struct
  type t =
  | Illegal of string
  | EOF
  | Int of int
  | Symbol of char
end

module Lexer = struct
  type t = {
    input: string;
    position: int;
    read_position: int;
    ch: char;
  }

  let null_byte = '\x00'

  let peek_char lex =
    if lex.read_position >= String.length lex.input then
      null_byte
    else
      String.get lex.input lex.read_position

  let read_char lex = 
    let ch = lex |> peek_char in
    {
      lex with
      position = lex.read_position;
      read_position = lex.read_position + 1;
      ch;
    }

  let make input = read_char { input; position = 0; read_position = 0; ch = null_byte }

  let is_digit ch = '0' <= ch && ch <= '9'

  let read_number lex =
    let rec go acc lex =
      if is_digit lex.ch then
        go (acc ^ String.make 1 lex.ch) (read_char lex)
      else
        (int_of_string acc, lex) in
    go "" lex

  let rec skip_whitespace lex =
    match lex.ch with
    | '.' | '\n' -> skip_whitespace (read_char lex)
    | _ -> lex

  let next_token lex =
    let lex = skip_whitespace lex in
    match lex.ch with
    | ch when ch = null_byte -> (lex.position, lex, Token.EOF)
    | ch when is_digit ch -> (
        let literal, lex' = (read_number lex) in
        (lex.position, lex', Token.Int literal)
      )
    | ch -> (lex.position, lex |> read_char, Token.Symbol ch)
end

let get_outline input start_pos end_pos =
  let line_length = String.index input '\n' in
  let input_length = String.length input in
  let top_left = max 0 (start_pos - 1 - line_length - 1) in
  let left = (start_pos - 1) in
  let top_right = max 0 (end_pos - line_length - 1) in
  let bottom_right = min (input_length - 1) (end_pos + line_length + 1) in
  let right = end_pos in
  let bottom_left = min (input_length - 1) (start_pos - 1 + line_length + 1) in
  (String.sub input top_left (top_right - top_left + 1))
  ^ (if left >= 0 then (String.make 1 (String.get input left)) else "")
  ^ (String.make 1 (String.get input right))
  ^ (String.sub input bottom_left (bottom_right - bottom_left + 1))

module Part_1 = struct
  let rec go acc lex =
    let (start, lex', token) = lex |> Lexer.next_token in
    match token with
    | Token.EOF -> acc
    | Token.Int number -> (
        let outline = get_outline lex.input start lex'.position |> Str.global_replace (Str.regexp "\n") "" in
        let is_candidate = (Str.string_match (Str.regexp "^[.0-9]+$") outline 0) = false in
        let candidate = if is_candidate then number else 0 in
        go (acc @ [candidate]) lex'
      )
    | _ -> go acc lex'

  let run (input : string) : (string, string) result = 
    let xs = (Lexer.make input) |> go [] in
    let sum = xs |> List.fold_left (+) 0 in
    Ok (sum |> Int64.of_int |> Int64.to_string)
end

let calc input pos x y =
  let length = String.length input in
  let line_length = String.index input '\n' + 1 in
  let pos' = pos + x + (y * line_length) in
  if pos' < 0 || pos' >= length then
    None
  else
    Some pos'
    
let rec find_start_position input pos = 
  let pos' = pos - 1 in
  if pos' < 0 then
    Lexer.make input
  else
    let ch = String.get input pos' in
    if Lexer.is_digit ch then
      find_start_position input pos'
    else
      {
        (Lexer.make input) with
        position = pos;
        read_position = pos + 1;
        ch = String.get input pos
      }

module Part_2 = struct
  let rec go acc lex = 
    let (start, lex', token) = lex |> Lexer.next_token in
    match token with
    | Token.EOF -> acc
    | Token.Symbol ch when ch = '*' -> go (start :: acc) lex'
    | _ -> go acc lex'

  let xs = [
    (-1, -1); (0, -1); (1, -1);
    (-1, 0); (1, 0);
    (-1, 1); (0, 1); (1, 1)
  ]

  let run (input : string) : (string, string) result =
    let gears = (go [] (Lexer.make input)) in
    let result = gears
      |> List.map (fun pos ->
        xs
        |> List.map (fun (x, y) -> calc input pos x y)
        |> List.filter_map (fun pos -> Option.bind pos (fun pos -> if String.get input pos = '.' then None else Some pos))
        |> List.map (fun x -> find_start_position input x)
        |> List.map (fun lex -> Lexer.next_token lex)
        |> List.filter_map (fun (start, lex, token) -> match token with
            | Token.Int number -> Some (start, lex, number)
            | _ -> None
          )
        |> List.sort_uniq (fun (s1, l1, _) (s2, l2, _) ->
            if (s1 < s2) then -1
            else if (s1 > s2) then 1
            else if (l1 < l2) then -1
            else if (l1 > l2) then 1
            else 0
          ) 
      )
      |> List.filter_map (function
            | (_, _, x1) :: (_, _, x2) :: [] -> Some (x1 * x2)
            | _ -> None
        )
      |> List.fold_left ( + ) 0
      |> Int64.of_int
      |> Int64.to_string
    in 
    Ok result
end
