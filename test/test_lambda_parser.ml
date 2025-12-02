open List
open Seq
open Coeffect_Sandbox
open Lexer
open Lambda.Parser
open Lambda.Types
open Test_core

let test_parse_empty () =
  Alcotest.(check lambda_prog)
    (Format.sprintf "Parser does not produce empty AST")
    None (parse Seq.empty)

let test_parser_inputs_simple =
  [
    (E_Var "x", "x");
    (E_Num 9, "9");
    (E_Abs ("x", E_Var "x"), "\\x.x");
    (E_App (E_Var "f", E_Var "x"), "f x");
  ]

let test_parser_inputs_application =
  [
    (* Application is left-associative *)
    (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), "f x y");
    (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), "(f x) y");
    (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), "((f x) y)");
    (E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z"), "f x y z");
    (E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z"), "(f x) y z");
    (E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z"), "((f x) y) z");
    (E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z"), "(((f x) y) z)");
    (* Parentheses correctly override associativity *)
    (E_App (E_Var "f", E_App (E_Var "x", E_Var "y")), "f (x y)");
    (E_App (E_App (E_Var "f", E_App (E_Var "x", E_Var "y")), E_Var "z"), "f (x y) z");
  ]

let test_parser_inputs_abstraction =
  [
    (E_Abs ("x", E_Var "x"), "(\\x.x)");
    (E_Abs ("x", E_Var "x"), "\\x.(x)");
    (E_Abs ("x", E_App (E_Var "x", E_Var "x")), "\\x.x x");
    (E_Abs ("x", E_App (E_Var "x", E_Var "x")), "\\x.(x x)");
    (E_Abs ("x", E_App (E_Var "x", E_Var "x")), "(\\x.x x)");
    (E_App (E_Abs ("x", E_Var "x"), E_Var "x"), "(\\x.x) x");
    (E_Abs ("x", E_Abs ("y", E_App (E_Var "x", E_Var "y"))), "\\x.\\y.x y");
  ]

let test_parse_ast (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let tester () =
    Alcotest.(check lambda_prog)
      (Format.sprintf "Did not produce expected for '%s'" inputs_str)
      (Some expected) (parse tokens_seq)
  in
  let name = Format.sprintf "Parser generates valid AST for '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let test_parser_inputs_fail =
  [
    ("Invalid Syntax. Reached EOF prematurely", "\\");
    ("Invalid Syntax. Expected a named binding", "\\.");
    ("Invalid Syntax. Reached EOF prematurely", "\\x");
    ("Invalid Syntax. Expected '.'", "\\x x");
    ("Invalid Syntax. Reached EOF prematurely", "(69");
    ("Invalid Syntax. Reached EOF prematurely", "((69)");
    ("Invalid Syntax. Reached EOF prematurely", "(((69))");
    ("Invalid Syntax. Expected EOF", "((69)))");
  ]

let test_parse_fail (msg, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let tester () =
    Alcotest.check_raises "Did not raise exception" (Failure msg) (fun () ->
        let _ = parse tokens_seq in
        ())
  in
  let name = Format.sprintf "Parser raises exception for invalid program '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let suite =
  [ Alcotest.test_case "Parser generates empty prog" `Quick test_parse_empty ]
  @ List.map test_parse_ast test_parser_inputs_simple
  @ List.map test_parse_ast test_parser_inputs_application
  @ List.map test_parse_ast test_parser_inputs_abstraction
  @ List.map test_parse_fail test_parser_inputs_fail
