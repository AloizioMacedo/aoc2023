let get_distance h total_time = (total_time - h) * h

type race = { total_time : int; distance : int }

let get_winning_hs (race : race) : int =
  let rec winning_hs r i =
    if i >= r.distance then 0
    else if get_distance i race.total_time > race.distance then
      1 + winning_hs r (i + 1)
    else winning_hs r (i + 1)
  in

  winning_hs race 0

let get_winnin_hs_via_bhaskara (race : race) : int =
  let b =
    (float_of_int race.total_time
    +. sqrt
         ((float_of_int race.total_time *. float_of_int race.total_time)
         -. (4.0 *. float_of_int race.distance)))
    /. 2.0
  in

  let a =
    (float_of_int race.total_time
    -. sqrt
         ((float_of_int race.total_time *. float_of_int race.total_time)
         -. (4.0 *. float_of_int race.distance)))
    /. 2.0
  in

  int_of_float (floor b -. ceil a) + 1

let solve_part_one (races : race list) : int =
  List.fold_left (fun acc x -> acc * get_winning_hs x) 1 races

let solve_part_two (races : race list) : int =
  List.fold_left (fun acc x -> acc * get_winnin_hs_via_bhaskara x) 1 races

let solve_part_one_test =
  let races =
    [
      { total_time = 7; distance = 9 };
      { total_time = 15; distance = 40 };
      { total_time = 30; distance = 200 };
    ]
  in

  solve_part_one races

let solve_part_one_real =
  let races =
    [
      { total_time = 40; distance = 219 };
      { total_time = 81; distance = 1012 };
      { total_time = 77; distance = 1365 };
      { total_time = 72; distance = 1089 };
    ]
  in

  solve_part_one races

let solve_part_two_test =
  let races = [ { total_time = 71530; distance = 940200 } ] in

  solve_part_two races

let solve_part_two_real =
  let races = [ { total_time = 40817772; distance = 219101213651089 } ] in

  solve_part_two races

let () = Printf.printf "Test Input P1: %d\n" solve_part_one_test
let () = Printf.printf "Real Input P1: %d\n" solve_part_one_real
let () = Printf.printf "Test Input P1: %d\n" solve_part_two_test
let () = Printf.printf "Real Input P1: %d\n" solve_part_two_real
