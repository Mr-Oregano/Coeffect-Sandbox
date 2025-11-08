open Coeffect_Sandbox

let pp_token fmt tk =
  match tk with
  | Lexer.LParen -> Format.fprintf fmt "LParen"
  | Lexer.RParen -> Format.fprintf fmt "RParen"
  | Lexer.Slash -> Format.fprintf fmt "Slash"
  | Lexer.Period -> Format.fprintf fmt "Period"
  | Lexer.Variable v -> Format.fprintf fmt "Var (%s)" v
  | Lexer.Literal n -> Format.fprintf fmt "Literal (%d)" n

let token = Alcotest.testable pp_token (fun t1 t2 -> t1 = t2)
let seq_token = Alcotest.seq token
