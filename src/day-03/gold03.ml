open Core_kernel.Std

type input = int

type answer = int

let string_of_input = string_of_int

let string_of_answer = string_of_int

let cases = [
  (1, 2);
  (2, 4);
  (3, 4);
  (4, 5);
  (5, 10);
  (6, 10);
  (312051, 312453);
]

let solve max =
  let matrix = Hashtbl.Poly.of_alist_exn [((0,0), 1)] in

  let do_step (x, y) = (
    let sum =
      (Option.value ~default:0 (Hashtbl.find matrix (x + 1, y + 1))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x + 1, y + 0))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x + 1, y - 1))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x + 0, y + 1))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x + 0, y - 1))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x - 1, y + 1))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x - 1, y + 0))) +
      (Option.value ~default:0 (Hashtbl.find matrix (x - 1, y - 1))) in
    if sum > max then
      Some sum
    else (
      Hashtbl.set matrix ~key:(x,y) ~data:sum;
      None
    )
  ) in

  let rec do_steps cnt start (dx, dy) = (match cnt with
    | 0 -> None
    | _ ->
      match do_step start with
        | Some _ as result -> result
        | None -> do_steps (cnt - 1) (fst start + dx, snd start + dy) (dx, dy)
  ) in

  let rec fill (x, y) = (
    let steps_cnt = 2 * Int.max (abs x) (abs y) in

    match do_steps steps_cnt (x *  1, y *  1) (0,  1) with
      | Some sum -> sum
      | None ->
    match do_steps steps_cnt (y * -1, x *  1) (-1, 0) with
      | Some sum -> sum
      | None ->
    match do_steps steps_cnt (x * -1, y * -1) (0, -1) with
      | Some sum -> sum
      | None ->
    match do_steps steps_cnt (y *  1, x * -1) (1,  0) with
      | Some sum -> sum
      | None -> fill (x + 1, y - 1)
  ) in

  fill (1, 0)