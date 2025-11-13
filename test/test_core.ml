open Coeffect_Sandbox

let pp_token fmt tk = Format.fprintf fmt "%s" (Lexer.token_to_string tk)
let token = Alcotest.testable pp_token (fun t1 t2 -> t1 = t2)
let pp_prog fmt p = Format.fprintf fmt "%s" (Parser.prog_to_string p)
let prog = Alcotest.testable pp_prog (fun (a1 : Parser.prog) a2 -> a1 = a2)
let seq_token = Alcotest.seq token
