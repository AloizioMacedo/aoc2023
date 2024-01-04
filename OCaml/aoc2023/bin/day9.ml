let test_input = Aoc2023.Read_file.read_file "input/day9/test_input.txt"
let _input = Aoc2023.Read_file.read_file "input/day9/input.txt"

let parse_line (line : string) : int list =
  List.map (fun x -> int_of_string x) (String.split_on_char ' ' line)

let parse_contents (contents : string list) : int list list =
  List.map parse_line contents

let rec diff l1 l2 =
  match (l1, l2) with
  | h1 :: t1, h2 :: t2 -> (h2 - h1) :: diff t1 t2
  | _, [] -> []
  | _ -> assert false

let rec last l =
  match l with [ x ] -> x | _ :: t -> last t | _ -> assert false

let rec forecast (l : int list) : int =
  if List.for_all (fun x -> x = 0) l then 0
  else let t = List.tl l in

       let diffs = diff l t in

       last t + forecast diffs

let solve_part_one (contents : string list) : int =
  let contents = parse_contents contents in

  let results = List.map (fun x -> forecast x) contents in

  List.fold_left (fun acc x -> acc + x) 0 results

let () = Printf.printf "Test Input P1: %d\n" (solve_part_one test_input)
let () = Printf.printf "Test Input P2: %d\n" (solve_part_one _input)
