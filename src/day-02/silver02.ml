open Core_kernel.Std

type input = string
type answer = int

let string_of_input i = i

let string_of_answer = string_of_int

let cases = [
  (String.concat ~sep:"\n" [
    "5 1 9 5";
    "7 5 3";
    "2 4 6 8";
  ], 18);
  (In_channel.read_all("./inputs/day-02.txt"), 47136);
]

let solve input =
  let lines = String.split_lines input in
  List.fold_left lines ~init:0 ~f:(fun sum line ->
    let row = String.split_on_chars ~on:['\t'; ' '] line in
    let nums = List.map row ~f:int_of_string in
    match nums with
      | [] -> sum
      | hd::tl ->
        let (min_v, max_v) = List.fold_left tl
          ~init:(hd, hd)
          ~f:(fun (min_v, max_v) num -> (min min_v num, max max_v num))
        in
        sum + max_v - min_v
  )
