#use "topfind";;
#thread;;
#require "core.top";;
#require "str";;

open Core.Std

let calc_checksum chan =
  In_channel.fold_lines chan ~init:0 ~f:(fun sum line ->
    let row = Str.split (Str.regexp "[ \t]+") line in
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

let () =
  let answer = calc_checksum In_channel.stdin in
  Out_channel.output_string Out_channel.stdout (string_of_int answer)
