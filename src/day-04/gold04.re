open Core_kernel.Std;

type input = list(string);

type answer = int;

let string_of_input: input => string = String.concat(~sep="\n");

let string_of_answer: answer => string = Int.to_string;

let cases: list((input, answer)) = [
  (["abcde fghij"], 1),
  (["abcde xyz ecdab"], 0),
  (["a ab abc abd abf abj"], 1),
  (["iiii oiii ooii oooi oooo"], 1),
  (["oiii ioii iioi iiio"], 0),
  (In_channel.read_lines("./inputs/day-04.txt"), 251)
];

module Char_compare =
  Comparator.Make(
    {
      type t = Char.Map.t(int);
      let sexp_of_t = Char.Map.sexp_of_t(Int.sexp_of_t);
      let t_of_sexp = Char.Map.t_of_sexp(Int.t_of_sexp);
      let compare = Char.Map.compare(Int.compare);
    }
  );

let isValid = pass =>
  if (String.lowercase(pass) != pass) {
    false;
  } else {
    let words =
      String.split_on_chars(pass, ~on=[' '])
      |> List.map(~f=String.to_list)
      |> List.map(~f=chars =>
           List.fold_left(chars, ~init=Char.Map.empty, ~f=(map, char) =>
             Map.update(map, char, ~f=count =>
               switch count {
               | None => 1
               | Some(c) => c + 1
               }
             )
           )
         );
    let uniq_count =
      Set.of_list(words, ~comparator=Char_compare.comparator) |> Set.length;
    uniq_count == List.length(words);
  };

let solve = passphrases =>
  List.fold_left(passphrases, ~init=0, ~f=(sum, pass) =>
    isValid(pass) ? sum + 1 : sum
  );