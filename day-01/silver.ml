#use "topfind";;
#thread;;
#require "core.top";;

open Core.Std

let int_of_char c = int_of_string (String.of_char c)

let solve_captcha = function
  | [] -> 0
  | (first :: rest as chars) ->
    let rec calc sum chars = match chars with
      | [] -> sum
      | [last] -> if last = first then sum + (int_of_char last) else sum
      | c1::(c2::_ as chars) -> if c1 = c2 then calc (sum + (int_of_char c1)) chars else calc sum chars
    in
    calc 0 chars

let () =
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "Expecting input at stdin"
  | Some captcha ->
    let answer = solve_captcha (String.to_list captcha) in
    Out_channel.output_string Out_channel.stdout (string_of_int answer)
