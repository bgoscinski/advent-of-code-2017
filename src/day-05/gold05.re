open Core_kernel.Std;

type input = list(string);

type answer = int;

let string_of_input: input => string = String.concat(~sep="\n");

let string_of_answer: answer => string = Int.to_string;

let cases: list((input, answer)) = [
  (["0", "3", "0", "1", "-3"], 10),
  (In_channel.read_lines("./inputs/day-05.txt"), 31150702)
];

let solve = list => {
  let maze = list |> Array.of_list |> Array.map(~f=Int.of_string);
  let len = Array.length(maze);
  let rec do_step = (maze, pos, step) =>
    if (pos >= len || pos < 0) {
      step;
    } else {
      let jmp = maze[pos];
      let diff = jmp >= 3 ? -1 : 1;
      maze[pos] = jmp + diff;
      do_step(maze, pos + jmp, step + 1);
    };
  do_step(maze, 0, 0);
};