let test_input = Aoc2023.Read_file.read_file "input/day1/test_input.txt"
let test_input_p2 = Aoc2023.Read_file.read_file "input/day1/test_input_p2.txt"
let input = Aoc2023.Read_file.read_file "input/day1/input.txt"
let rec last x = match x with [] -> None | [ x ] -> Some x | _ :: t -> last t

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let is_digit x : bool = '0' <= x && x <= '9'

let get_num (x : string) : int =
  let chars = explode x in

  let filtered = List.filter is_digit chars in

  let first = List.hd filtered in
  let last = last filtered in

  ((int_of_char first - int_of_char '0') * 10)
  + int_of_char (Option.get last)
  - int_of_char '0'

let rec sum x = match x with [] -> 0 | [ x ] -> x | h :: t -> h + sum t
let () = Printf.printf "Test Input P1: %d\n" (sum (List.map get_num test_input))
let () = Printf.printf "Real Input P2: %d\n" (sum (List.map get_num input))

let extract_num (x : string) : int option =
  if String.starts_with ~prefix:"one" x || String.starts_with ~prefix:"1" x then
    Some 1
  else if String.starts_with ~prefix:"two" x || String.starts_with ~prefix:"2" x
  then Some 2
  else if
    String.starts_with ~prefix:"three" x || String.starts_with ~prefix:"3" x
  then Some 3
  else if
    String.starts_with ~prefix:"four" x || String.starts_with ~prefix:"4" x
  then Some 4
  else if
    String.starts_with ~prefix:"five" x || String.starts_with ~prefix:"5" x
  then Some 5
  else if String.starts_with ~prefix:"six" x || String.starts_with ~prefix:"6" x
  then Some 6
  else if
    String.starts_with ~prefix:"seven" x || String.starts_with ~prefix:"7" x
  then Some 7
  else if
    String.starts_with ~prefix:"eight" x || String.starts_with ~prefix:"8" x
  then Some 8
  else if
    String.starts_with ~prefix:"nine" x || String.starts_with ~prefix:"9" x
  then Some 9
  else None

let extract_nums (x : string) : int list =
  let rec extract (x : string) (i : int) =
    if i >= String.length x then []
    else
      match extract_num (String.sub x i (String.length x - i)) with
      | None -> extract x (i + 1)
      | Some y -> y :: extract x (i + 1)
  in
  extract x 0

let get_num_part_two (x : string) : int =
  let nums = extract_nums x in

  let first = List.hd nums in
  let last = last nums in

  (first * 10) + Option.get last

let () =
  Printf.printf "Test Input P1: %d\n"
    (sum (List.map get_num_part_two test_input_p2))

let () =
  Printf.printf "Real Input P2: %d\n" (sum (List.map get_num_part_two input))
