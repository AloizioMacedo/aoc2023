let test_input = Aoc2023.Read_file.read_file "input/day3/test_input.txt"
let input = Aoc2023.Read_file.read_file "input/day3/input.txt"

let get_nrows_and_ncols (contents : string list) : int * int =
  let rows = List.length contents in
  let cols = String.length (List.hd contents) in
  (rows, cols)

let build_matrix (contents : string list) : char array array =
  let rows, cols = get_nrows_and_ncols contents in
  let matrix = Array.make_matrix rows cols ' ' in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      matrix.(i).(j) <- String.get (List.nth contents i) j
    done
  done;

  matrix
