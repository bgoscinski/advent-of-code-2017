open Core_kernel.Std;

type input = list(string);

type answer = string;

let string_of_input: input => string = String.concat(~sep="\n");

let string_of_answer: answer => string = Fn.id;

let cases: list((input, answer)) = [
  (
    [
      "pbga (66)",
      "xhth (57)",
      "ebii (61)",
      "havc (66)",
      "ktlj (57)",
      "fwft (72) -> ktlj, cntj, xhth",
      "qoyq (66)",
      "padx (45) -> pbga, havc, qoyq",
      "tknk (41) -> ugml, padx, fwft",
      "jptl (61)",
      "ugml (68) -> gyxo, ebii, jptl",
      "gyxo (61)",
      "cntj (57)"
    ],
    "tknk"
  ),
  (In_channel.read_lines("./inputs/day-07.txt"), "qibuqqg")
];

type prog = {
  name: string,
  weight: int,
  holds: list(string)
};

let solve = lines => {
  let progs =
    lines
    |> List.map(~f=line => {
         let name = line |> String.split_on_chars(~on=[' ']) |> List.hd_exn;
         let weight =
           line
           |> String.split_on_chars(~on=['(', ')'])
           |> List.tl_exn
           |> List.hd_exn
           |> Int.of_string;
         let holds =
           if (String.contains(line, '>')) {
             line
             |> String.split_on_chars(~on=['>'])
             |> List.rev
             |> List.hd_exn
             |> String.split_on_chars(~on=[',', ' '])
             |> List.filter(~f=str => ! String.equal(str, ""));
           } else {
             [];
           };
         {name, weight, holds};
       })
    |> List.fold(~init=String.Map.empty, ~f=(map, prog) =>
         switch prog.holds {
         | [] => map
         | _ => Map.add(map, ~key=prog.name, ~data=prog)
         }
       );
  let reduced =
    Map.fold(progs, ~init=progs, ~f=(~key as _, ~data as sample, progs) =>
      List.fold(sample.holds, ~init=progs, ~f=(progs, progName) =>
        Map.remove(progs, progName)
      )
    );
  if (Map.length(reduced) == 1) {
    (Map.min_elt_exn(reduced) |> snd).name;
  } else {
    failwith("something bad");
  };
};