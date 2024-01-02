let test_input = Aoc2023.Read_file.read_file "input/day2/test_input.txt"
let input = Aoc2023.Read_file.read_file "input/day2/input.txt"

type gameset = { red : int; green : int; blue : int }
type game = { id : int; gamesets : gameset list }

module Colors = struct
  type t = string

  let compare x y = String.compare x y
end

module ColorMap = Map.Make (Colors)

let parse_gameset (s : string) : gameset =
  let parse_color (s : string) : string * int =
    let s = String.trim s in
    let num, color =
      match String.split_on_char ' ' s with
      | num :: colors -> (String.trim num, List.hd colors)
      | _ -> assert false
    in

    let num = int_of_string num in
    let color = String.trim color in

    (color, num)
  in

  let colors = String.split_on_char ',' s in

  let colors_map =
    List.fold_left
      (fun map color ->
        let color, num = parse_color color in

        ColorMap.add color num map)
      ColorMap.empty colors
  in

  let red =
    match ColorMap.find_opt "red" colors_map with Some num -> num | None -> 0
  in

  let green =
    match ColorMap.find_opt "green" colors_map with
    | Some num -> num
    | None -> 0
  in

  let blue =
    match ColorMap.find_opt "blue" colors_map with Some num -> num | None -> 0
  in

  { red; green; blue }

let parse_line (line : string) : game =
  let game, sets =
    match String.split_on_char ':' line with
    | [ game; sets ] -> (game, sets)
    | _ -> assert false
  in

  let id =
    match String.split_on_char ' ' game with
    | [ _; id ] -> int_of_string (String.trim id)
    | _ -> assert false
  in

  let sets = String.split_on_char ';' sets in

  let gamesets = List.map parse_gameset sets in

  { id; gamesets }

let parse_all_games (lines : string list) : game list =
  List.map parse_line lines

let is_game_valid (game : game) : bool =
  List.for_all
    (fun set -> set.red <= 12 && set.green <= 13 && set.blue <= 14)
    game.gamesets

let solve_part_one (lines : string list) : int =
  let games = parse_all_games lines in

  List.filter is_game_valid games
  |> List.fold_left (fun acc game -> acc + game.id) 0

let get_maxs (game : game) : int * int * int =
  List.fold_left
    (fun (max_red, max_green, max_blue) set ->
      (max set.red max_red, max set.green max_green, max set.blue max_blue))
    (0, 0, 0) game.gamesets

let solve_part_two (lines : string list) : int =
  let games = parse_all_games lines in

  List.fold_left
    (fun acc x ->
      let max_red, max_green, max_blue = get_maxs x in
      acc + (max_red * max_green * max_blue))
    0 games

let () = Printf.printf "Test Input P1: %d\n" (solve_part_one test_input)
let () = Printf.printf "Real Input P1: %d\n" (solve_part_one input)
let () = Printf.printf "Test Input P2: %d\n" (solve_part_two test_input)
let () = Printf.printf "Real Input P2: %d\n" (solve_part_two input)
