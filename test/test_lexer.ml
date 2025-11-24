open List
open Seq
open Coeffect_Sandbox
open Lexer
open Token
open Test_core

(* TODO: Fix on fail printer! Currently it just throws exception *)
(* TODO: Parameterize the fail check, check for more failures *)

let test_lex_empty () =
  Alcotest.(check seq_token)
    (Format.sprintf "Lex does not produce empty sequence")
    empty (lex empty)

let test_lex_fail () =
  Alcotest.check_raises "Did not raise exception"
    (Failure "Unexpected character") (fun () ->
      iter (fun _ -> ()) (lex (singleton '$')))

let test_lex_inputs_single =
  [
    ([ T_LParen ], "(");
    ([ T_RParen ], ")");
    ([ T_Slash ], "\\");
    ([ T_Period ], ".");
    ([ T_Var "x" ], "x");
    ([ T_Num 6 ], "6");
  ]

let test_lex_inputs_standard =
  [
    ([ T_LParen; T_LParen ], "((");
    ([ T_RParen; T_RParen ], "))");
    ([ T_LParen; T_Period; T_RParen ], "(.)");
    ([ T_LParen; T_Slash; T_RParen ], "(\\)");
    ([ T_LParen; T_Slash; T_Period; T_RParen ], "(\\.)");
  ]

let test_lex_inputs_spaces =
  [
    ([ T_LParen; T_RParen ], " ( \t )\n ");
    ([ T_LParen; T_LParen ], "(  (\n \t");
    ([ T_RParen; T_RParen ], "  \t)\n )");
    ([ T_LParen; T_Period; T_RParen ], "\t( . )");
    ([ T_LParen; T_Slash; T_RParen ], "  ( \\\t)\n");
    ([ T_LParen; T_Slash; T_Period; T_RParen ], "\n\n(   \\\t.)");
  ]

let test_lex_inputs_vars_and_literals =
  [
    ([ T_Num 123 ], "123");
    ([ T_Num 123456 ], "123456");
    ([ T_Num 123; T_Num 456 ], "123 456");
    ([ T_Num 123; T_Period; T_Num 456 ], "123.456");
    ([ T_Var "abc123" ], "abc123");
    ([ T_Var "a"; T_Var "bc123" ], "a bc123");
    ([ T_Var "abc"; T_Num 123 ], " \nabc\n123");
  ]

let test_lex_inputs_complete =
  [
    ([ T_Slash; T_Var "x"; T_Period; T_Var "x" ], "\\x.x");
    ( [
        T_LParen; T_Slash; T_Var "x"; T_Period; T_Var "x"; T_RParen; T_Num 69420;
      ],
      "(\\x. x) 69420" );
  ]

let test_lex_inputs_in_channel =
  [
    ( [
        T_LParen; T_Slash; T_Var "x"; T_Period; T_Var "x"; T_RParen; T_Num 69420;
      ],
      "(\\x. x) 69420" );
  ]

let test_lex_inputs_implicit_parameters =
  [
    ([ T_Colon ], ":");
    ([ T_Semicolon ], ";");
    ([ T_Arrow ], "->");
    ([ T_Equals ], "=");
    ([ T_Plus ], "+");
    ([ T_Fun ], "fun");
    ([ T_IntTyp ], "int");
    ([ T_UnitTyp ], "unit");
    ([ T_LetDyn ], "letdyn");
    ([ T_In ], "in");
    ([ T_UnitVal ], "()");
    ([ T_Exclamation ], "!");
    ([ T_ImpVar "?x" ], "?x");
    ([ T_Var "x" ], "# This is a comment\nx");
    ([ T_LCurly ], "{");
    ([ T_RCurly ], "}");
    ([ T_Comma ], ",");
  ]

let test_lex_toks (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let inputs_seq = once (String.to_seq inputs) in
  let expected_seq = to_seq expected in
  let tester () =
    Alcotest.(check seq_token)
      (Format.sprintf "Did not produce expected for '%s'" inputs_str)
      expected_seq (lex inputs_seq)
  in
  let name =
    Format.sprintf "Lexer generates valid tokens for '%s'" inputs_str
  in
  Alcotest.test_case name `Quick tester

let suite =
  [ Alcotest.test_case "Lexer generates empty sequence" `Quick test_lex_empty ]
  @ [ Alcotest.test_case "Lexer generates failures" `Quick test_lex_fail ]
  @ List.map test_lex_toks test_lex_inputs_single
  @ List.map test_lex_toks test_lex_inputs_standard
  @ List.map test_lex_toks test_lex_inputs_spaces
  @ List.map test_lex_toks test_lex_inputs_vars_and_literals
  @ List.map test_lex_toks test_lex_inputs_complete
  @ List.map test_lex_toks test_lex_inputs_implicit_parameters
