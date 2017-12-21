open Core_kernel.Std;

type input = string;

type answer = string;

let string_of_input = Fn.id;

let string_of_answer = Fn.id;

let cases: list((input, answer)) = [];

let solve = Fn.id;