open List
open Seq
open Coeffect_Sandbox
open Lexer
open Parser
open Interpreter
open Test_core

let test_interpret_empty () =
  Alcotest.(check res)
    (Format.sprintf "Parser does not produce empty AST")
    None (eval None)

let test_interpret_fail () =
  Alcotest.check_raises "Did not raise exception"
    (Failure "Runtime Error. Cannot apply to non function-type") (fun () ->
      let _ = eval (parse (lex (once (String.to_seq "69 420")))) in
      ())

let test_interpret_inputs_simple =
  [ (Num 69420, "69420"); (Clo ("x", Var "x", []), "\\x.x") ]

let test_interpret_inputs_application =
  [
    (Num 69420, "(\\x.x) 69420");
    (Num 69420, "(\\x.\\y.x y) (\\z.z) 69420");
    ( Clo ("y", App (Var "x", Var "y"), [ ("x", Clo ("z", Var "z", [])) ]),
      "(\\x.\\y.x y) (\\z.z)" );
  ]

let test_interpret_prog (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let prog = parse tokens_seq in
  let tester () =
    Alcotest.(check res)
      (Format.sprintf "Did not produce expected for '%s'" inputs_str)
      (Some expected) (eval prog)
  in
  let name =
    Format.sprintf "Interp resolves to valid value for '%s'" inputs_str
  in
  Alcotest.test_case name `Quick tester

let suite =
  [ Alcotest.test_case "Interp resolves to no-op" `Quick test_interpret_empty ]
  @ [ Alcotest.test_case "Interp raises exception" `Quick test_interpret_fail ]
  @ List.map test_interpret_prog test_interpret_inputs_simple
  @ List.map test_interpret_prog test_interpret_inputs_application
