let file = "input.txt"

let first_digit str =
  let rec first_digit_from_index str index =
    if index < String.length str then
      try Some (int_of_string (String.make 1 str.[index]))
      with Failure _ -> first_digit_from_index str (index + 1)
    else None
  in
  first_digit_from_index str 0

let last_digit str =
  let rec last_digit_from_index str index =
    if index >= 0 then
      try Some (int_of_string (String.make 1 str.[index]))
      with Failure _ -> last_digit_from_index str (index - 1)
    else None
  in
  last_digit_from_index str (String.length str - 1)

let first_last str =
  match first_digit str with
  | None -> 0
  | Some x -> ( (x * 10) + match last_digit str with None -> 0 | Some y -> y)

let () =
  let ic = open_in file in
  let rec sum_of_lines () =
    match input_line ic with
    | line -> first_last line + sum_of_lines ()
    | exception End_of_file ->
        close_in ic;
        0
  in
  let answer = sum_of_lines () in
  print_endline (string_of_int answer);
  flush stdout
