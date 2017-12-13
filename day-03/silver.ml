#use "topfind";;
#thread;;
#require "core.top";;

open Core.Std

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

let () =
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "Expecting input at stdin"
  | Some input ->
    let answer = solve (Int.of_string input) in
    Out_channel.output_string Out_channel.stdout (string_of_int answer)
