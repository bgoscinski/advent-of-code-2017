#use "topfind";;
#thread;;
#require "core.top";;

open Core.Std

let int_of_char c = int_of_string (String.of_char c)

let split_at idx list =
  let rec split idx l1 l2 =
    if idx = 0 then (List.rev l1, l2) else
    match l2 with
      | [] -> (List.rev l1, l2)
      | hd::tl -> split (idx - 1) (hd :: l1) tl
  in
    split idx [] list

let solve_captcha chars =
  let (half1, half2) = split_at ((List.length chars) / 2) chars in
  List.fold2_exn chars (half2 @ half1)
    ~f:(fun (sum: int) (c1: char) (c2: char) -> if c1 = c2 then sum + (int_of_char c1) else sum)
    ~init:0

let () =
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "Expecting input at stdin"
  | Some captcha ->
    let answer = solve_captcha (String.to_list captcha) in
    Out_channel.output_string Out_channel.stdout (string_of_int answer)
