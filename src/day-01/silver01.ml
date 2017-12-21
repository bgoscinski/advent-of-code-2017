open Core_kernel.Std

type input = string
type answer = int

let string_of_input i = i

let string_of_answer = string_of_int

let cases: (input * answer) list = [
  ("1122", 3);
  ("1111", 4);
  ("1234", 0);
  ("91212129", 9);
  (In_channel.read_all("./inputs/day-01.txt"), 1141)
]

let int_of_char c = int_of_string (String.of_char c)

let solve input =
  match String.to_list input with
  | [] -> 0
  | (first :: _ as chars) ->
    let rec calc sum chars = match chars with
      | [] -> sum
      | [last] -> if last = first then sum + (int_of_char last) else sum
      | c1::(c2::_ as chars) -> if c1 = c2 then calc (sum + (int_of_char c1)) chars else calc sum chars
    in
    calc 0 chars
