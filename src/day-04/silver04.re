open Core_kernel.Std;

type input = list(string);

type answer = int;

let string_of_input: input => string = String.concat(~sep="\n");

let string_of_answer: answer => string = Int.to_string;

let cases: list((input, answer)) = [
  (["aa bb cc dd ee"], 1),
  (["aa bb cc dd aa"], 0),
  (["aa bb cc dd aaa"], 1),
  (["aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa"], 2),
  (In_channel.read_lines("./inputs/day-04.txt"), 466)
];

let isValid = pass =>
  if (String.lowercase(pass) != pass) {
    false;
  } else {
    let words = String.split_on_chars(pass, ~on=[' ']);
    Set.length(Set.Poly.of_list(words)) == List.length(words);
  };

let solve = passphrases =>
  List.fold_left(passphrases, ~init=0, ~f=(sum, pass) =>
    isValid(pass) ? sum + 1 : sum
  );