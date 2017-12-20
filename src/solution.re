module type Solution = {
  type input;
  type answer;
  let string_of_input: input => string;
  let string_of_answer: answer => string;
  let cases: list((input, answer));
  let solve: input => answer;
};

module Test = (S: Solution) => {
  let check = () =>
    S.cases
    |> List.iter(((input, expected)) => {
         let actual = S.solve(input);
         if (actual == expected) {
           ();
         } else {
           Printf.printf(
             "Expected solve(%s) == %s but got %s\n",
             S.string_of_input(input),
             S.string_of_answer(expected),
             S.string_of_answer(actual)
           );
         };
       });
};