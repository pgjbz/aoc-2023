let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let is_char_digit (ch: char) =
  match ch with
  | '1'| '2'| '3'| '4'| '5'| '6'| '7'| '8'| '9'| '0' -> true
  | _ -> false
;;

let explode s = List.init (String.length s) (String.get s)

let only_numbers list: char list = list |> List.filter is_char_digit

let first_and_last chs = [List.nth chs 0] @ [List.nth chs ((List.length chs) - 1)];;

let char_to_string chs = String.concat "" (List.map (String.make 1) chs)

let parse str = try int_of_string str with Failure _ -> 0

let rec sum ns = match ns with
  | []  -> 0
  | n::ns ->  n + sum ns


let replace input reg rep = 
  Str.global_replace reg rep input

let rec full_replace str replaces = 
    match replaces with 
    | [] -> str
    | (reg, v)::ns -> full_replace (replace str reg v) ns 

let str_replace str = 
  let list_of_replaces: (Str.regexp * string) list = 
    [(Str.regexp {|nine|}, "n9e")] @
    [(Str.regexp {|eight|}, "e8t")] @
    [(Str.regexp {|seven|}, "s7n")] @
    [(Str.regexp {|six|}, "s6x")] @
    [(Str.regexp {|five|}, "f5e")] @
    [(Str.regexp {|four|}, "f4r")] @
    [(Str.regexp {|three|}, "t3e")] @
    [(Str.regexp {|two|}, "t2o")] @
    [(Str.regexp {|one|}, "o1e")] 
  in
  full_replace str list_of_replaces

let () = 
  let list_ns = read_lines "true-input-2.txt" |>
  List.map str_replace |> (*comment this line to run easy challange*)
  List.map explode |> 
  List.map only_numbers |>
  List.map first_and_last |>
  List.map char_to_string |>
  List.map parse in
  let result = sum list_ns in
  print_int result


