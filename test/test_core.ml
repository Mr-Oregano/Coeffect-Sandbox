open Coeffect_Sandbox

let pp_token fmt tk = Format.fprintf fmt "%s" (Lexer.token_to_string tk)
let token = Alcotest.testable pp_token (fun t1 t2 -> t1 = t2)
let seq_token = Alcotest.seq token
let pp_prog fmt p = Format.fprintf fmt "%s" (Parser.prog_to_string p)
let prog = Alcotest.testable pp_prog (fun p1 p2 -> p1 = p2)
let pp_res fmt r = Format.fprintf fmt "%s" (Interpreter.res_to_string r)
let res = Alcotest.testable pp_res (fun r1 r2 -> r1 = r2)
