open List
open Seq
open Coeffect_Sandbox
open Lexer
open Parser
open Test_core

let test_parse_empty () =
  Alcotest.(check prog)
    (Format.sprintf "Parser does not produce empty AST")
    None (parse Seq.empty)

let test_parser_inputs_simple =
  [
    (Var "x", "x");
    (Num 9, "9");
    (Abs ("x", Var "x"), "\\x.x");
    (App (Var "f", Var "x"), "f x");
  ]

let test_parser_inputs_application =
  [
    (* Application is left-associative *)
    (App (App (Var "f", Var "x"), Var "y"), "f x y");
    (App (App (Var "f", Var "x"), Var "y"), "(f x) y");
    (App (App (Var "f", Var "x"), Var "y"), "((f x) y)");
    (App (App (App (Var "f", Var "x"), Var "y"), Var "z"), "f x y z");
    (App (App (App (Var "f", Var "x"), Var "y"), Var "z"), "(f x) y z");
    (App (App (App (Var "f", Var "x"), Var "y"), Var "z"), "((f x) y) z");
    (App (App (App (Var "f", Var "x"), Var "y"), Var "z"), "(((f x) y) z)");
    (* Parentheses correctly override associativity *)
    (App (Var "f", App (Var "x", Var "y")), "f (x y)");
    (App (App (Var "f", App (Var "x", Var "y")), Var "z"), "f (x y) z");
  ]

let test_parser_inputs_abstraction =
  [
    (Abs ("x", Var "x"), "(\\x.x)");
    (Abs ("x", Var "x"), "\\x.(x)");
    (Abs ("x", App (Var "x", Var "x")), "\\x.x x");
    (Abs ("x", App (Var "x", Var "x")), "\\x.(x x)");
    (Abs ("x", App (Var "x", Var "x")), "(\\x.x x)");
    (App (Abs ("x", Var "x"), Var "x"), "(\\x.x) x");
    (Abs ("x", Abs ("y", App (Var "x", Var "y"))), "\\x.\\y.x y");
  ]

let test_parse_ast (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let tester () =
    Alcotest.(check prog)
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
  let name =
    Format.sprintf "Parser raises exception for invalid program '%s'" inputs_str
  in
  Alcotest.test_case name `Quick tester

let suite =
  [ Alcotest.test_case "Parser generates empty prog" `Quick test_parse_empty ]
  @ List.map test_parse_ast test_parser_inputs_simple
  @ List.map test_parse_ast test_parser_inputs_application
  @ List.map test_parse_ast test_parser_inputs_abstraction
  @ List.map test_parse_fail test_parser_inputs_fail
