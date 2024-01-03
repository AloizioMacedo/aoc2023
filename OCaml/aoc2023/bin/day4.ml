let test_input = Aoc2023.Read_file.read_file "input/day4/test_input.txt"
let input = Aoc2023.Read_file.read_file "input/day4/input.txt"

type card = { winning_numbers : int list; my_numbers : int list }

let parse_line (line : string) : card =
  let _, rest =
    match String.split_on_char ':' line with
    | [ x; y ] -> (x, y)
    | _ -> failwith "parse error"
  in

  let winning_numbers, my_numbers =
    match String.split_on_char '|' rest with
    | [ x; y ] -> (x, y)
    | _ -> failwith "parse error"
  in

  let winning_numbers = String.split_on_char ' ' winning_numbers in
  let winning_numbers = List.filter (fun x -> x <> "") winning_numbers in

  let winning_numbers =
    List.map (fun x -> String.trim x |> int_of_string) winning_numbers
  in

  let my_numbers = String.split_on_char ' ' my_numbers in
  let my_numbers = List.filter (fun x -> x <> "") my_numbers in

  let my_numbers =
    List.map (fun x -> String.trim x |> int_of_string) my_numbers
  in

  { my_numbers; winning_numbers }

let rec compute_score_from_count (n : int) : int =
  if n = 0 then 0 else if n = 1 then 1 else 2 * compute_score_from_count (n - 1)

let count_winning (card : card) : int =
  List.fold_left
    (fun acc my_number ->
      if List.mem my_number card.winning_numbers then acc + 1 else acc)
    0 card.my_numbers

let get_score (card : card) : int =
  let count = count_winning card in

  compute_score_from_count count

let rec compute_final_cards (cards : card list) (i : int)
    (current_list : int list) : int list =
  if i >= List.length cards then current_list
  else
    let card = List.nth cards i in

    let number_of_cards = List.nth current_list i in
    let score = count_winning card in

    let new_list =
      List.mapi
        (fun j x -> if j > i && j <= i + score then x + number_of_cards else x)
        current_list
    in

    compute_final_cards cards (i + 1) new_list

let solve_part_one (contents : string list) : int =
  let cards = List.map parse_line contents in
  List.fold_left (fun acc x -> acc + get_score x) 0 cards

let solve_part_two (contents : string list) : int =
  let cards = List.map parse_line contents in
  let final_cards = compute_final_cards cards 0 (List.map (fun _ -> 1) cards) in

  List.fold_left (fun acc x -> acc + x) 0 final_cards

let () = Printf.printf "Test Input P1: %d\n" (solve_part_one test_input)
let () = Printf.printf "Real Input P1: %d\n" (solve_part_one input)
let () = Printf.printf "Test Input P2: %d\n" (solve_part_two test_input)
let () = Printf.printf "Test Input P2: %d\n" (solve_part_two input)
