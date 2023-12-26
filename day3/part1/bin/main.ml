let file = "input.txt"

type element = Num of int | Symbol | Blank

let get_at m x y =
  if x >= 0 && y >= 0 && y < List.length m && x < List.length (List.nth m y) then
    List.nth (List.nth m y) x
  else Blank

let parse_char ch =
  match ch with
  | '0' .. '9' -> Num (int_of_char ch - int_of_char '0')
  | '.' -> Blank
  | _ -> Symbol

let parse_str s =
  List.init (String.length s) (fun i -> parse_char (String.get s i))

let is_near_symbol m x y =
  get_at m (x - 1) (y - 1) = Symbol
  || get_at m x (y - 1) = Symbol
  || get_at m (x + 1) (y - 1) = Symbol
  || get_at m (x - 1) y = Symbol
  || get_at m (x + 1) y = Symbol
  || get_at m (x - 1) (y + 1) = Symbol
  || get_at m x (y + 1) = Symbol
  || get_at m (x + 1) (y + 1) = Symbol

(* let print_str_list l = String.concat "," l |> print_endline *)
(* let print_int_list l = List.map string_of_int l |> print_str_list *)

(* let print_matrix l =
   let string_array = List.map (List.map (fun e -> match e with Num n -> string_of_int n | Blank -> "." | Symbol -> "*")) l in
   String.concat "\n" (List.map (String.concat ",") string_array)
   |> print_endline *)

let () =
  let ic = open_in file in
  let rec get_matrix () =
    match input_line ic with
    | line -> parse_str line :: get_matrix ()
    | exception End_of_file ->
        close_in ic;
        []
  in
  let mat = get_matrix () in
  let process_x m x y =
    let rec aux is_found prev init m x y =
      if x <= (List.nth m y |> List.length) then
        let current = get_at m x y in
        match current with
        | Num n ->
            let curr = (prev * 10) + n in
            let found = is_found || is_near_symbol m x y in
            aux found curr init m (x + 1) y
        | _ ->
            let sum = init + if is_found then prev else 0 in
            aux false 0 sum m (x + 1) y
      else init
    in
    aux false 0 0 m x y
  in
  let process m =
    let rec process_y m y =
      if y < List.length m then process_x m 0 y + process_y m (y + 1) else 0
    in
    process_y m 0
  in
  process mat |> string_of_int |> print_endline;
  (* process_y mat 0 0 |> print_str_list; *)
  (* print_matrix mat; *)
  flush stdout
