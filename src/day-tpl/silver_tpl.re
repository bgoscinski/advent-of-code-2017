open Core_kernel.Std;

type input = string;

type answer = string;

let string_of_input: input => string = Fn.id;

let string_of_answer: answer => string = Fn.id;

let cases: list((input, answer)) = [];

let solve: input => answer = Fn.id;