let year = 2023;
let day = 2;

let is_digit = (c) => {
  switch (c) {
  | '0' .. '9' => true
  | _ => false
  }
}

type cube = Red(int) | Blue(int) | Green(int)

let parse_cube = (input) =>
  switch (String.split_on_char(' ', input)) {
  | [count, color] => {
      let count = count |> Int64.of_string |> Int64.to_int;
      switch color {
      | "red" => Red(count)
      | "blue" => Blue(count)
      | "green" => Green(count)
      | _ => raise(Invalid_argument(""))
      }
    }
  | _ => raise(Invalid_argument(""))
  }

let stringify = (input) => {
  switch input {
  | Red(n) => String.concat("", ["Red(", n |> Int64.of_int |> Int64.to_string, ")"])
  | Blue(n) => String.concat("", ["Blue(", n |> Int64.of_int |> Int64.to_string, ")"])
  | Green(n) => String.concat("", ["Green(", n |> Int64.of_int |> Int64.to_string, ")"])
  }
}

let parse_set = (input) =>
  input |> Str.split(Str.regexp(", ")) |> List.map(parse_cube)

let parse_bag = (input) =>
  input |>  Str.split(Str.regexp("; ")) |> List.map(parse_set)

type game = Game(int, list(list(cube)))

let parse_game = (input): game =>
  switch (input |>  Str.split(Str.regexp(": "))) {
  | [id, set] => switch (id |> Str.split(Str.regexp(" "))) {
    | ["Game", n] => Game(n |> Int64.of_string |> Int64.to_int, parse_bag(set))
    | _ => raise(Invalid_argument(""))
    }
  | _ => raise(Invalid_argument(""))
  }

let check_cube = (cube) =>
  switch cube {
  | Red(n) when n <= 12 => Some(Red(n))
  | Green(n) when n <= 13 => Some(Green(n))
  | Blue(n) when n <= 14 => Some(Blue(n))
  | _ => None
  }

let rec sequence = (xs: list(option('a))): option(list('a)) =>
  switch xs {
  | [Some(x)] => Some([x])
  | [Some(x), ...tl] => sequence(tl) |> Option.map((tl) => [x, ...tl])
  | [None, ..._] => None
  | [] => None
  }

let parse_game_with_rule = (game: game): option(game) =>
  switch (game) {
  | Game(id, set) => {
      set |> List.map(List.map(check_cube)) |> List.map(sequence) |> sequence |> Option.map((x) => Game(id, x))
    }
  }

type bag = {
  red: int,
  green: int,
  blue: int,
}

let find_max_cubes = (bag: bag, cubes: list(cube)) =>
  cubes |> List.fold_left((acc, cube) => {
    switch (cube) {
    | Red(n) => { ...acc, red: Int.max(n, acc.red) }
    | Green(n) => { ...acc, green: Int.max(n, acc.green) }
    | Blue(n) => { ...acc, blue: Int.max(n, acc.blue) }
    }
  }, bag)

module Part_1 = {
  let run = (input: string): result(string, string) => {
    let lines = input |> String.split_on_char('\n') |> List.filter((line) => String.length(line) > 0);

    Ok(lines |> List.map(parse_game) |> List.map(parse_game_with_rule) |> List.filter_map(Option.map((Game(id, _)) => id)) |> List.fold_left((+), 0) |> Int64.of_int |> Int64.to_string);
  }
};

module Part_2 = {
  let run = (input: string): result(string, string) => {
    let lines = input |> String.split_on_char('\n') |> List.filter((line) => String.length(line) > 0);
    let result = lines
      |> List.map(parse_game)
      |> List.map((Game(_, cubes)) => cubes |> List.concat)
      |> List.map(find_max_cubes({ red: 0, green: 0, blue: 0 }))
      |> List.map(({ red, green, blue }) => red * green * blue)
      |> List.fold_left((+), 0)
      |> Int64.of_int
      |> Int64.to_string

    Ok(result);
  }
};
