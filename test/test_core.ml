open Coeffect_Sandbox

let pp_token fmt tk = Format.fprintf fmt "%s" (Lexer.token_to_string tk)
let token = Alcotest.testable pp_token (fun t1 t2 -> t1 = t2)
let seq_token = Alcotest.seq token

(* Lambda Calculus *)
let lambda_pp_prog fmt p =
  Format.fprintf fmt "%s" (Lambda.Parser.prog_to_string p)

let lambda_prog = Alcotest.testable lambda_pp_prog (fun p1 p2 -> p1 = p2)

let lambda_pp_res fmt r =
  Format.fprintf fmt "%s" (Lambda.Interpreter.res_to_string r)

let lambda_res = Alcotest.testable lambda_pp_res (fun r1 r2 -> r1 = r2)

(* Implicit Parameters *)
let implicit_pp_prog fmt p =
  Format.fprintf fmt "%s" (Implicit.Parser.prog_to_string p)

let implicit_prog = Alcotest.testable implicit_pp_prog (fun p1 p2 -> p1 = p2)
