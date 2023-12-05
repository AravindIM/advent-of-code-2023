let file = "input.txt"

let checker s =
  let data = s |> String.trim |> String.split_on_char ' ' in
  let colour = List.nth data 1 in
  let count = List.nth data 0 |> String.trim |> int_of_string in
  match colour with
  | "red" -> count <= 12
  | "green" -> count <= 13
  | "blue" -> count <= 14
  | _ -> false

let is_correct_attempt str =
  let groups = String.split_on_char ',' str in
  let rec aux = function
      | h::t -> if checker h then aux t else false
      | _ -> true in
  aux groups

let is_correct_game str =
  let attempts = String.split_on_char ';' str in
  let rec aux = function
    | h::t -> if is_correct_attempt h then aux t else false
    | _ -> true in
  aux attempts

let possible_game_num str =
  try
    let first_split = String.split_on_char ':' str in
    let game_num = (List.hd first_split |> String.split_on_char ' ' |> List.nth) 1 |> int_of_string in
    let game_data = List.nth first_split 1 in
    if is_correct_game game_data then game_num else 0
  with Failure _ -> 0

let () =
  let ic = open_in file in
  let rec sum_of_lines () =
    match input_line ic with
    | line -> possible_game_num line + sum_of_lines ()
    | exception End_of_file ->
        close_in ic;
        0
  in
  let answer = sum_of_lines () in
  string_of_int answer |> print_endline;
  flush stdout
