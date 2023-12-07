let file = "input.txt"

let add a b =
  match (a, b) with
  | ((a1, a2, a3), (b1, b2, b3)) -> (a1 + b1, a2 + b2, a3 + b3)

let combine_max_counts a b =
  match (a, b) with
  | ((a1, a2, a3), (b1, b2, b3)) -> (max a1 b1, max a2 b2, max a3 b3)

let colour_count_single s =
  let data = s |> String.trim |> String.split_on_char ' ' in
  let colour = List.nth data 1 in
  let count = List.nth data 0 |> String.trim |> int_of_string in
  match colour with
  | "red" -> (count, 0, 0)
  | "green" -> (0, count, 0)
  | "blue" -> (0, 0, count)
  | _ -> (0, 0, 0)

let colour_count_attempt str =
  let groups = String.split_on_char ',' str in
  let rec aux group_info prev = match group_info with
      | h::t -> colour_count_single h |> add prev |> aux t
      | _ -> prev in
  aux groups (0,0,0)

let max_count_game str =
  let attempts = String.split_on_char ';' str in
  let rec aux attempt_info prev = match attempt_info with
    | h::t -> colour_count_attempt h |> combine_max_counts prev |> aux t
    | _ -> prev in
  aux attempts (0, 0, 0)

let power_of_game str =
  try
    let first_split = String.split_on_char ':' str in
    let game_data = List.nth first_split 1 in
    let max_count = max_count_game game_data in
    match max_count with
    | (r, g, b) -> r * g * b
  with Failure _ -> 0

let () =
  let ic = open_in file in
  let rec sum_of_lines () =
    match input_line ic with
    | line -> power_of_game line + sum_of_lines ()
    | exception End_of_file ->
        close_in ic;
        0
  in
  let answer = sum_of_lines () in
  string_of_int answer |> print_endline;
  flush stdout
