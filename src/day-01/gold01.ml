open Core_kernel.Std

type input = string
type answer = int

let string_of_input i = i

let string_of_answer = string_of_int

let cases: (input * answer) list = [
  ("1212", 6);
  ("1221", 0);
  ("123425", 4);
  ("123123", 12);
  ("12131415", 4);
  (In_channel.read_all("./inputs/day-01.txt"), 950);
]

let int_of_char c = int_of_string (String.of_char c)

let split_at idx list =
  let rec split idx l1 l2 =
    if idx = 0 then (List.rev l1, l2) else
    match l2 with
      | [] -> (List.rev l1, l2)
      | hd::tl -> split (idx - 1) (hd :: l1) tl
  in
    split idx [] list

let solve input =
  let chars = String.to_list input in
  let (half1, half2) = split_at ((List.length chars) / 2) chars in
  List.fold2_exn chars (half2 @ half1)
    ~f:(fun (sum: int) (c1: char) (c2: char) -> if c1 = c2 then sum + (int_of_char c1) else sum)
    ~init:0
