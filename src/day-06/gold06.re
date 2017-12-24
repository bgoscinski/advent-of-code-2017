open Core_kernel.Std;

type input = string;

type answer = int;

let string_of_input: input => string = Fn.id;

let string_of_answer: answer => string = Int.to_string;

let cases: list((input, answer)) = [
  (String.concat(["0", "2", "7", "0"], ~sep="\t"), 4),
  (List.hd_exn(In_channel.read_lines("./inputs/day-06.txt")), 1610)
];

module IntMapCmp =
  Comparator.Make(
    {
      type t = Int.Map.t(int);
      let sexp_of_t = Int.Map.sexp_of_t(Int.sexp_of_t);
      let t_of_sexp = Int.Map.t_of_sexp(Int.t_of_sexp);
      let compare = Int.Map.compare(Int.compare);
    }
  );

let solve = input => {
  let mem =
    input
    |> String.split_on_chars(~on=['\t'])
    |> List.map(~f=Int.of_string)
    |> List.foldi(~init=Int.Map.empty, ~f=(bank, map, blocks) =>
         Map.add(map, ~key=bank, ~data=blocks)
       );
  let seen = Map.singleton(~comparator=IntMapCmp.comparator, mem, 0);
  let rec realloc =
    fun
    | (mem, _, 0) => mem
    | (mem, bank, blocks) => {
        let bank = bank mod Map.length(mem);
        let map2 =
          Map.change(
            mem,
            bank,
            ~f=
              fun
              | None => None
              | Some(x) => Some(x + 1)
          );
        realloc((map2, bank + 1, blocks - 1));
      };
  let rec pass = (seen, mem, step) => {
    let (maxBank, maxBlocks) =
      Map.fold(
        mem,
        ~init=Map.min_elt_exn(mem),
        ~f=(~key as bank, ~data as blocks, (maxBank, maxBlocks) as max) =>
        if (blocks > maxBlocks) {
          (bank, blocks);
        } else if (blocks == maxBlocks && bank < maxBank) {
          (bank, blocks);
        } else {
          max;
        }
      );
    let mem = Map.add(mem, ~key=maxBank, ~data=0);
    let nextMem = realloc((mem, maxBank + 1, maxBlocks));
    if (Map.mem(seen, nextMem)) {
      let prevStep = Map.find_exn(seen, nextMem);
      step - prevStep;
    } else {
      let seen = Map.add(seen, ~key=nextMem, ~data=step);
      pass(seen, nextMem, step + 1);
    };
  };
  pass(seen, mem, 1);
};