let file = "input.txt"

let convert_to_digit ch = 
  match ch with
| '0'..'9' -> Some(int_of_char ch - int_of_char '0')
| _ -> None

let first_digit str =
  let length = String.length str in
  let rec first_digit_from_index index =
    if index < length then
      match convert_to_digit str.[index] with
      | Some digit -> digit
      | None -> index + 1 |> first_digit_from_index
    else 0
  in
  first_digit_from_index 0

let last_digit str =
  let length = String.length str in
  let rec last_digit_from_index index =
    if index >= 0 then
      match convert_to_digit str.[index] with
      | Some(digit) -> digit
      | None -> index - 1 |> last_digit_from_index
    else 0
  in
  length -1 |> last_digit_from_index

let first_last str = 
  first_digit str * 10 + last_digit str

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
  string_of_int answer |> print_endline;
  flush stdout
