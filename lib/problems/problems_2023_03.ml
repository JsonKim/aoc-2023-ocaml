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

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
