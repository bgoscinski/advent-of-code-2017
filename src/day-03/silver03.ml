open Core_kernel.Std

type input = int

type answer = int

let string_of_input = string_of_int

let string_of_answer = string_of_int

let cases = [
  (1, 0);
  (12, 3);
  (23, 2);
  (1024, 31);
  (312051, 430);
]

let side shell = 2.0 *. shell +. 1.0

let area shell = (side shell) ** 2.0

let solve = function
  | 1 -> 0
  | i ->
    let i = (float_of_int i) in
    let shell = Float.round_up ((sqrt i -. 1.0) /. 2.0) in
    let idx = i -. area (shell -. 1.0) in
    let side = side shell in
    let half = Float.iround_down_exn (side /. 2.0) in
    let norm = Float.iround_down_exn (Float.mod_float idx (side -. 1.0)) in
    abs (norm - half) + half
