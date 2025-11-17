open List
open Seq
open Coeffect_Sandbox
open Lexer
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
    ([ LParen ], "(");
    ([ RParen ], ")");
    ([ Slash ], "\\");
    ([ Period ], ".");
    ([ Variable "x" ], "x");
    ([ Literal 6 ], "6");
  ]

let test_lex_inputs_standard =
  [
    ([ LParen; RParen ], "()");
    ([ LParen; LParen ], "((");
    ([ RParen; RParen ], "))");
    ([ LParen; Period; RParen ], "(.)");
    ([ LParen; Slash; RParen ], "(\\)");
    ([ LParen; Slash; Period; RParen ], "(\\.)");
  ]

let test_lex_inputs_spaces =
  [
    ([ LParen; RParen ], " ( \t )\n ");
    ([ LParen; LParen ], "(  (\n \t");
    ([ RParen; RParen ], "  \t)\n )");
    ([ LParen; Period; RParen ], "\t( . )");
    ([ LParen; Slash; RParen ], "  ( \\\t)\n");
    ([ LParen; Slash; Period; RParen ], "\n\n(   \\\t.)");
  ]

let test_lex_inputs_vars_and_literals =
  [
    ([ Literal 123 ], "123");
    ([ Literal 123456 ], "123456");
    ([ Literal 123; Literal 456 ], "123 456");
    ([ Literal 123; Period; Literal 456 ], "123.456");
    ([ Variable "abc123" ], "abc123");
    ([ Variable "a"; Variable "bc123" ], "a bc123");
    ([ Variable "abc"; Literal 123 ], " \nabc\n123");
  ]

let test_lex_inputs_complete =
  [
    ([ Slash; Variable "x"; Period; Variable "x" ], "\\x.x");
    ( [
        LParen; Slash; Variable "x"; Period; Variable "x"; RParen; Literal 69420;
      ],
      "(\\x. x) 69420" );
  ]

let test_lex_inputs_in_channel =
  [
    ( [
        LParen; Slash; Variable "x"; Period; Variable "x"; RParen; Literal 69420;
      ],
      "(\\x. x) 69420" );
  ]

let test_lex_inputs_implicit_parameters =
  [
    ([ Colon ], ":");
    ([ Semicolon ], ";");
    ([ Arrow ], "->");
    ([ Equals ], "=");
    ([ Plus ], "+");
    ([ KW_Fun ], "fun");
    ([ KW_Int ], "int");
    ([ KW_Unit ], "unit");
    ([ KW_Letdyn ], "letdyn");
    ([ KW_In ], "in");
    ([ Implicit "?x" ], "?x");
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
