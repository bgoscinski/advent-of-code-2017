open Core_kernel.Std

type input = string
type answer = int

let string_of_input i = i

let string_of_answer = string_of_int

let cases = [
  (String.concat ~sep:"\n" [
    "5 9 2 8";
    "9 4 7 3";
    "3 8 6 5";
  ], 9);
  (In_channel.read_all("./inputs/day-02.txt"), 250);
]

let rec find_with_rest list f = match list with
  | [] -> None
  | hd::tl -> match f hd tl with
    | (Some _ as res) -> res
    | None -> find_with_rest tl f

let solve input =
  let lines = String.split_lines input in
  List.fold_left lines ~init:0 ~f:(fun sum line ->
    let row = String.split_on_chars ~on:['\t'; ' '] line in
    let nums = List.map row ~f:int_of_string in

    let res = find_with_rest nums (fun x rest ->
      List.find_map rest ~f:(fun y ->
        if x <> 0 && (y % x) = 0 then Some (y, x)
        else if y <> 0 && (x % y) = 0 then Some (x, y)
        else None
      )
    ) in

    match res with
      | None -> sum
      | Some (x, y) -> sum + (x / y)
  )