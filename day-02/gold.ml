#use "topfind";;
#thread;;
#require "core.top";;
#require "str";;

open Core.Std

let rec find_with_rest list f = match list with
  | [] -> None
  | hd::tl -> match f hd tl with
    | (Some _ as res) -> res
    | None -> find_with_rest tl f

let calc_checksum chan =
  In_channel.fold_lines chan ~init:0 ~f:(fun sum line ->
    let row = Str.split (Str.regexp "[ \t]+") line in
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

let () =
  let answer = calc_checksum In_channel.stdin in
  Out_channel.output_string Out_channel.stdout (string_of_int answer)
