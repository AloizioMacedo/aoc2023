let test_input = Aoc2023.Read_file.read_file "input/day7/test_input.txt"
let input = Aoc2023.Read_file.read_file "input/day7/input.txt"

let card_of_string x =
  match x with
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> assert false

let card_of_string_with_joker x =
  match x with
  | 'J' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'T' -> 10
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> assert false

type hand = int * int * int * int * int
type bid = { hand : hand; bid : int }

let _print_hand hand =
  let a, b, c, d, e = hand in
  let () = Printf.printf "%d %d %d %d %d\n" a b c d e in
  ()

let parse_line (line : string) : bid =
  let hand, bid =
    match String.split_on_char ' ' line with
    | [ x; y ] -> (x, y)
    | _ -> assert false
  in

  let hand = hand |> String.to_seq |> List.of_seq in
  let a, b, c, d, e =
    match hand with
    | [ a; b; c; d; e ] ->
        ( card_of_string a,
          card_of_string b,
          card_of_string c,
          card_of_string d,
          card_of_string e )
    | _ -> assert false
  in

  let bid = int_of_string bid in

  { hand = (a, b, c, d, e); bid }

let parse_line_with_joker (line : string) : bid =
  let hand, bid =
    match String.split_on_char ' ' line with
    | [ x; y ] -> (x, y)
    | _ -> assert false
  in

  let hand = hand |> String.to_seq |> List.of_seq in
  let a, b, c, d, e =
    match hand with
    | [ a; b; c; d; e ] ->
        ( card_of_string_with_joker a,
          card_of_string_with_joker b,
          card_of_string_with_joker c,
          card_of_string_with_joker d,
          card_of_string_with_joker e )
    | _ -> assert false
  in

  let bid = int_of_string bid in

  { hand = (a, b, c, d, e); bid }

let get_counts hand =
  let a, b, c, d, e = hand in
  let counts = Array.make 15 0 in

  counts.(a) <- counts.(a) + 1;
  counts.(b) <- counts.(b) + 1;
  counts.(c) <- counts.(c) + 1;
  counts.(d) <- counts.(d) + 1;
  counts.(e) <- counts.(e) + 1;

  counts

let get_max_idx (arr : int array) : int =
  let rec get_max_idx (arr : int array) (current_max_idx : int) (i : int) : int
      =
    if i == Array.length arr then current_max_idx
    else
      let new_max_idx =
        if arr.(i) > arr.(current_max_idx) then i else current_max_idx
      in
      get_max_idx arr new_max_idx (i + 1)
  in

  get_max_idx arr 2 2

let is_five_of_a_kind hand =
  let counts = get_counts hand in

  Array.exists (fun x -> x == 5) counts

let is_four_of_a_kind hand =
  let counts = get_counts hand in

  Array.exists (fun x -> x == 4) counts

let is_full_house hand =
  let counts = get_counts hand in

  Array.exists (fun x -> x == 3) counts && Array.exists (fun x -> x == 2) counts

let is_three_of_a_kind hand =
  let counts = get_counts hand in

  Array.exists (fun x -> x == 3) counts

let has_two_pair hand =
  let counts = get_counts hand in

  let counts = Array.to_list counts in

  List.length (List.filter (fun x -> x == 2) counts) >= 2

let has_pair hand =
  let counts = get_counts hand in

  Array.exists (fun x -> x == 2) counts

let fix_counts_with_joker (counts : int array) : int array =
  let max_idx = get_max_idx counts in

  counts.(max_idx) <- counts.(max_idx) + counts.(1);
  counts.(1) <- 0;

  counts

let is_five_of_a_kind_with_joker hand =
  let counts = get_counts hand in
  let counts = fix_counts_with_joker counts in

  Array.exists (fun x -> x == 5) counts

let is_four_of_a_kind_with_joker hand =
  let counts = get_counts hand in
  let counts = fix_counts_with_joker counts in

  Array.exists (fun x -> x == 4) counts

let is_full_house_with_joker hand =
  let counts = get_counts hand in
  let counts = fix_counts_with_joker counts in

  Array.exists (fun x -> x == 3) counts && Array.exists (fun x -> x == 2) counts

let is_three_of_a_kind_with_joker hand =
  let counts = get_counts hand in
  let counts = fix_counts_with_joker counts in

  Array.exists (fun x -> x == 3) counts

let has_two_pair_with_joker hand =
  let counts = get_counts hand in
  let counts = fix_counts_with_joker counts in

  let counts = Array.to_list counts in

  List.length (List.filter (fun x -> x == 2) counts) >= 2

let has_pair_with_joker hand =
  let counts = get_counts hand in
  let counts = fix_counts_with_joker counts in

  Array.exists (fun x -> x == 2) counts

let compare hand1 hand2 =
  let a1, a2, a3, a4, a5 = hand1 in
  let b1, b2, b3, b4, b5 = hand2 in

  if is_five_of_a_kind hand1 && not (is_five_of_a_kind hand2) then 1
  else if (not (is_five_of_a_kind hand1)) && is_five_of_a_kind hand2 then -1
  else if is_four_of_a_kind hand1 && not (is_four_of_a_kind hand2) then 1
  else if (not (is_four_of_a_kind hand1)) && is_four_of_a_kind hand2 then -1
  else if is_full_house hand1 && not (is_full_house hand2) then 1
  else if (not (is_full_house hand1)) && is_full_house hand2 then -1
  else if is_three_of_a_kind hand1 && not (is_three_of_a_kind hand2) then 1
  else if (not (is_three_of_a_kind hand1)) && is_three_of_a_kind hand2 then -1
  else if has_two_pair hand1 && not (has_two_pair hand2) then 1
  else if (not (has_two_pair hand1)) && has_two_pair hand2 then -1
  else if has_pair hand1 && not (has_pair hand2) then 1
  else if (not (has_pair hand1)) && has_pair hand2 then -1
  else if a1 > b1 then 1
  else if a1 < b1 then -1
  else if a2 > b2 then 1
  else if a2 < b2 then -1
  else if a3 > b3 then 1
  else if a3 < b3 then -1
  else if a4 > b4 then 1
  else if a4 < b4 then -1
  else if a5 > b5 then 1
  else if a5 < b5 then -1
  else 0

let compare_with_joker hand1 hand2 =
  let a1, a2, a3, a4, a5 = hand1 in
  let b1, b2, b3, b4, b5 = hand2 in

  if
    is_five_of_a_kind_with_joker hand1
    && not (is_five_of_a_kind_with_joker hand2)
  then 1
  else if
    (not (is_five_of_a_kind_with_joker hand1))
    && is_five_of_a_kind_with_joker hand2
  then -1
  else if
    is_four_of_a_kind_with_joker hand1
    && not (is_four_of_a_kind_with_joker hand2)
  then 1
  else if
    (not (is_four_of_a_kind_with_joker hand1))
    && is_four_of_a_kind_with_joker hand2
  then -1
  else if is_full_house_with_joker hand1 && not (is_full_house_with_joker hand2)
  then 1
  else if
    (not (is_full_house_with_joker hand1)) && is_full_house_with_joker hand2
  then -1
  else if
    is_three_of_a_kind_with_joker hand1
    && not (is_three_of_a_kind_with_joker hand2)
  then 1
  else if
    (not (is_three_of_a_kind_with_joker hand1))
    && is_three_of_a_kind_with_joker hand2
  then -1
  else if has_two_pair_with_joker hand1 && not (has_two_pair_with_joker hand2)
  then 1
  else if (not (has_two_pair_with_joker hand1)) && has_two_pair_with_joker hand2
  then -1
  else if has_pair_with_joker hand1 && not (has_pair_with_joker hand2) then 1
  else if (not (has_pair_with_joker hand1)) && has_pair_with_joker hand2 then -1
  else if a1 > b1 then 1
  else if a1 < b1 then -1
  else if a2 > b2 then 1
  else if a2 < b2 then -1
  else if a3 > b3 then 1
  else if a3 < b3 then -1
  else if a4 > b4 then 1
  else if a4 < b4 then -1
  else if a5 > b5 then 1
  else if a5 < b5 then -1
  else 0

let parse_bids (contents : string list) : bid list =
  List.map parse_line contents

let parse_bids_with_joker (contents : string list) : bid list =
  List.map parse_line_with_joker contents

let solve_part_one (contents : string list) : int =
  let bids = parse_bids contents in

  let sorted_hands =
    List.sort (fun bid1 bid2 -> compare bid1.hand bid2.hand) bids
  in

  let rec aux sorted_hands i =
    match sorted_hands with h :: t -> (h.bid * i) + aux t (i + 1) | _ -> 0
  in

  aux sorted_hands 1

let solve_part_two (contents : string list) : int =
  let bids = parse_bids_with_joker contents in

  let sorted_hands =
    List.sort (fun bid1 bid2 -> compare_with_joker bid1.hand bid2.hand) bids
  in

  let rec aux sorted_hands i =
    match sorted_hands with h :: t -> (h.bid * i) + aux t (i + 1) | _ -> 0
  in

  aux sorted_hands 1

let () = Printf.printf "Test Input Part 1: %d\n" @@ solve_part_one test_input
let () = Printf.printf "Real Input Part 1: %d\n" @@ solve_part_one input
let () = Printf.printf "Test Input Part 2: %d\n" @@ solve_part_two test_input
let () = Printf.printf "Real Input Part 2: %d\n" @@ solve_part_two input
