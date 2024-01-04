let test_input = Aoc2023.Read_file.read_file "input/day8/test_input.txt"
let input = Aoc2023.Read_file.read_file "input/day8/input.txt"

module Node = struct
  type t = string

  let compare x y = String.compare x y
end

module NodeMap = Map.Make (Node)

type node = { id : string; left : string; right : string }
type direction = L | R

let direction_of_char c = match c with 'L' -> L | 'R' -> R | _ -> assert false

let parse_node (line : string) : node =
  let id, next_nodes =
    match String.split_on_char '=' line with
    | [ x; y ] -> (String.trim x, String.trim y)
    | _ -> assert false
  in

  let left, right =
    match String.split_on_char ',' next_nodes with
    | [ x; y ] ->
        ( String.sub x 1 (String.length x - 1),
          String.sub (String.trim y) 0 (String.length y - 2) )
    | _ -> assert false
  in

  { id; left; right }

let parse_directions (line : string) : direction list =
  let chars = String.to_seq line in

  List.of_seq @@ Seq.map direction_of_char chars

let rec drop (l : 'a list) (n : int) : 'a list =
  if n == 0 then l
  else match l with _ :: t -> drop t (n - 1) | _ -> assert false

let parse_contents (contents : string list) : direction list * node list =
  let first_line = List.nth contents 0 in
  let rest = drop contents 2 in

  let directions = parse_directions first_line in
  let nodes = List.map parse_node rest in

  (directions, nodes)

let nodes_to_map (nodes : node list) : node NodeMap.t =
  NodeMap.of_seq (nodes |> List.map (fun n -> (n.id, n)) |> List.to_seq)

let rec walk (directions : direction list) (node_map : node NodeMap.t)
    (current_node : string) (current_dir_idx : int) : int =
  if current_node = "ZZZ" then 0
  else
    let next = NodeMap.find current_node node_map in

    let direction =
      List.nth directions (current_dir_idx mod List.length directions)
    in

    match direction with
    | L -> 1 + walk directions node_map next.left (current_dir_idx + 1)
    | R -> 1 + walk directions node_map next.right (current_dir_idx + 1)

let solve_part_one (contents : string list) : int =
  let directions, nodes = parse_contents contents in

  let node_map = nodes_to_map nodes in

  walk directions node_map "AAA" 0

let () = Printf.printf "Test Input P1: %d\n" (solve_part_one test_input)
let () = Printf.printf "Real Input P1: %d\n" (solve_part_one input)
