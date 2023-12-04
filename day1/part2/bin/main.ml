let file = "input.txt"

let text_to_digit = function
| s when String.starts_with ~prefix:"zero" s || String.starts_with ~prefix:"0" s -> Some(0)
| s when String.starts_with ~prefix:"one" s || String.starts_with ~prefix:"1" s -> Some(1)
| s when String.starts_with ~prefix:"two" s || String.starts_with ~prefix:"2" s -> Some(2)
| s when String.starts_with ~prefix:"three" s || String.starts_with ~prefix:"3" s -> Some(3)
| s when String.starts_with ~prefix:"four" s || String.starts_with ~prefix:"4" s -> Some(4)
| s when String.starts_with ~prefix:"five" s || String.starts_with ~prefix:"5" s -> Some(5)
| s when String.starts_with ~prefix:"six" s || String.starts_with ~prefix:"6" s -> Some(6)
| s when String.starts_with ~prefix:"seven" s || String.starts_with ~prefix:"7" s -> Some(7)
| s when String.starts_with ~prefix:"eight" s || String.starts_with ~prefix:"8" s -> Some(8)
| s when String.starts_with ~prefix:"nine" s || String.starts_with ~prefix:"9" s -> Some(9)
| _ -> None

let first_digit str =
  let length = String.length str in
  let rec first_digit_from_index index =
    if index < length then
      match length - index |> String.sub str index |> text_to_digit with
      | Some digit -> digit
      | None -> index + 1 |> first_digit_from_index
    else 0
  in
  first_digit_from_index 0

let last_digit str =
  let length = String.length str in
  let rec last_digit_from_index index =
    if index >= 0 then
      match length - index |> String.sub str index |> text_to_digit with
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