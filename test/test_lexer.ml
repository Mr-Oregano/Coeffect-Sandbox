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
  Alcotest.check_raises "Did not raise exception" (Failure "Unexpected character") (fun () ->
      iter (fun _ -> ()) (lex (singleton '$')))

let test_lex_inputs_single =
  [
    ([ TK_LParen ], "(");
    ([ TK_RParen ], ")");
    ([ TK_Slash ], "\\");
    ([ TK_Period ], ".");
    ([ TK_Var "x" ], "x");
    ([ TK_Num 6 ], "6");
  ]

let test_lex_inputs_standard =
  [
    ([ TK_LParen; TK_LParen ], "((");
    ([ TK_RParen; TK_RParen ], "))");
    ([ TK_LParen; TK_Period; TK_RParen ], "(.)");
    ([ TK_LParen; TK_Slash; TK_RParen ], "(\\)");
    ([ TK_LParen; TK_Slash; TK_Period; TK_RParen ], "(\\.)");
  ]

let test_lex_inputs_spaces =
  [
    ([ TK_LParen; TK_RParen ], " ( \t )\n ");
    ([ TK_LParen; TK_LParen ], "(  (\n \t");
    ([ TK_RParen; TK_RParen ], "  \t)\n )");
    ([ TK_LParen; TK_Period; TK_RParen ], "\t( . )");
    ([ TK_LParen; TK_Slash; TK_RParen ], "  ( \\\t)\n");
    ([ TK_LParen; TK_Slash; TK_Period; TK_RParen ], "\n\n(   \\\t.)");
  ]

let test_lex_inputs_vars_and_literals =
  [
    ([ TK_Num 123 ], "123");
    ([ TK_Num 123456 ], "123456");
    ([ TK_Num 123; TK_Num 456 ], "123 456");
    ([ TK_Num 123; TK_Period; TK_Num 456 ], "123.456");
    ([ TK_Var "abc123" ], "abc123");
    ([ TK_Var "a"; TK_Var "bc123" ], "a bc123");
    ([ TK_Var "abc"; TK_Num 123 ], " \nabc\n123");
  ]

let test_lex_inputs_complete =
  [
    ([ TK_Slash; TK_Var "x"; TK_Period; TK_Var "x" ], "\\x.x");
    ( [ TK_LParen; TK_Slash; TK_Var "x"; TK_Period; TK_Var "x"; TK_RParen; TK_Num 69420 ],
      "(\\x. x) 69420" );
  ]

let test_lex_inputs_in_channel =
  [
    ( [ TK_LParen; TK_Slash; TK_Var "x"; TK_Period; TK_Var "x"; TK_RParen; TK_Num 69420 ],
      "(\\x. x) 69420" );
  ]

let test_lex_inputs_implicit_parameters =
  [
    ([ TK_Colon ], ":");
    ([ TK_Semicolon ], ";");
    ([ TK_Arrow ], "->");
    ([ TK_Equals ], "=");
    ([ TK_Plus ], "+");
    ([ TK_KW_Fun ], "fun");
    ([ TK_KW_Int ], "int");
    ([ TK_KW_Unit ], "unit");
    ([ TK_KW_LetDyn ], "letdyn");
    ([ TK_KW_In ], "in");
    ([ TK_Unit ], "()");
    ([ TK_Exclamation ], "!");
    ([ TK_ImpVar "?x" ], "?x");
    ([ TK_Var "x" ], "# This is a comment\nx");
    ([ TK_LCurly ], "{");
    ([ TK_RCurly ], "}");
    ([ TK_Comma ], ",");
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
  let name = Format.sprintf "Lexer generates valid tokens for '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let suite =
  [
    Alcotest.test_case "Lexer generates empty sequence" `Quick test_lex_empty;
    Alcotest.test_case "Lexer generates failures" `Quick test_lex_fail;
  ]
  @ List.map test_lex_toks test_lex_inputs_single
  @ List.map test_lex_toks test_lex_inputs_standard
  @ List.map test_lex_toks test_lex_inputs_spaces
  @ List.map test_lex_toks test_lex_inputs_vars_and_literals
  @ List.map test_lex_toks test_lex_inputs_complete
  @ List.map test_lex_toks test_lex_inputs_implicit_parameters
