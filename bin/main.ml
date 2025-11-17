open Coeffect_Sandbox
open Extchannel

let interpret (in_ : in_channel) =
  let tokens = Lexer.lex (In_channel.to_seq in_) in
  let prog = Lambda.Parser.parse tokens in
  Lambda.Interpreter.eval prog

(* Driver *)
(* TODO: Implement sys-arg-based cmdline interpreter *)
let () =
  let result =
    In_channel.with_open_text "supplemental/samples/lex.lc" interpret
  in
  print_endline (Lambda.Interpreter.res_to_string result)
