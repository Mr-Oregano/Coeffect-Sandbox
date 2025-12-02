open Coeffect_Sandbox
open Extchannel

let interpret (in_ : in_channel) =
  let tokens = Lexer.lex (In_channel.to_seq in_) in
  let ast = Implicit.Parser.parse tokens in
  let prog = Implicit.Typecheck.type_check ast in
  (* Lambda.Interpreter.eval prog *)
  prog

(* Driver *)
(* TODO: Implement sys-arg-based cmdline interpreter *)
let _ =
  let result =
    In_channel.with_open_text
      "supplemental/implicit-parameters/samples/sample.ip" interpret
  in
  (* print_endline (Lambda.Interpreter.res_to_string result) *)
  result
