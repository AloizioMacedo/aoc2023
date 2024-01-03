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

let solve_part_one (races : race list) : int =
  List.fold_left (fun acc x -> acc * get_winning_hs x) 1 races

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

let () = Printf.printf "Test Input P1: %d\n" solve_part_one_test
let () = Printf.printf "Real Input P1: %d\n" solve_part_one_real
