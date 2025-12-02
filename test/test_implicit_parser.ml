open List
open Seq
open Coeffect_Sandbox
open Lexer
open Implicit.Parser
open Implicit.Types.Ast
open Test_core

let test_parser_inputs_simple =
  [
    (([], E_Var "x"), "; x");
    (([], E_Num 9), "; 9");
    (([], E_ImpVar "?x"), "; ?x");
    (([], E_UnitVal), "; ()");
    (([], E_App (E_Var "f", E_Var "x")), "; f x");
    (([], E_Add (E_Var "x", E_Var "y")), "; x + y");
    (([], E_LetDyn { imp = "?x"; init = E_Num 9; body = E_ImpVar "?x" }), "; letdyn ?x = 9 in ?x");
  ]

let test_parser_inputs_application =
  [
    (* Application is left-associative *)
    (([], E_App (E_App (E_Var "f", E_Var "x"), E_Var "y")), "; f x y");
    (([], E_App (E_App (E_Var "f", E_Var "x"), E_Var "y")), "; (f x) y");
    (([], E_App (E_App (E_Var "f", E_Var "x"), E_Var "y")), "; ((f x) y)");
    (([], E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z")), "; f x y z");
    (([], E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z")), "; (f x) y z");
    (([], E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z")), "; ((f x) y) z");
    (([], E_App (E_App (E_App (E_Var "f", E_Var "x"), E_Var "y"), E_Var "z")), "; (((f x) y) z)");
    (* Parentheses correctly override associativity *)
    (([], E_App (E_Var "f", E_App (E_Var "x", E_Var "y"))), "; f (x y)");
    (([], E_App (E_App (E_Var "f", E_App (E_Var "x", E_Var "y")), E_Var "z")), "; f (x y) z");
  ]

let test_parser_inputs_val_decls =
  [
    (([ D_Val ("x", E_Num 9) ], E_Var "x"), "val x = 9 ; x");
    ( ([ D_Val ("x", E_Num 6); D_Val ("y", E_Num 9) ], E_Add (E_Var "x", E_Var "y")),
      "val x = 6 val y = 9 ; x + y" );
  ]

let test_parser_inputs_fun_decls =
  [
    ( ( [
          D_Fun
            { name = "f"; params = [ ("x", T_Int) ]; imps = []; ret_typ = T_Int; body = E_Var "x" };
        ],
        E_App (E_Var "f", E_Num 9) ),
      "fun f (x: int): int = x ; f 9" );
  ]

let test_parser_inputs_add =
  [
    (* Addition is left-associative *)
    (([], E_Add (E_Add (E_Var "x", E_Var "y"), E_Var "z")), "; x + y + z");
    (([], E_Add (E_Add (E_Var "x", E_Var "y"), E_Var "z")), "; (x + y) + z");
    (([], E_Add (E_Add (E_Var "x", E_Var "y"), E_Var "z")), "; ((x + y) + z)");
    (* Parentheses override associativity *)
    (([], E_Add (E_Var "x", E_Add (E_Var "y", E_Var "z"))), "; x + (y + z)");
  ]

let test_parser_inputs_params =
  [
    ( ( [
          D_Fun
            {
              name = "f";
              params =
                [
                  ("a", T_Int);
                  ("b", T_UnitTyp);
                  ("d", T_Func { from = T_Int; to_ = T_UnitTyp; imps = [] });
                ];
              imps = [];
              ret_typ = T_Int;
              body = E_Var "x";
            };
        ],
        E_App (E_Var "f", E_Num 9) ),
      "fun f (a: int) (b: unit) (d: int -> unit): int = x ; f 9" );
    ( ( [
          D_Fun
            {
              name = "f";
              params = [ ("a", T_Func { from = T_Int; to_ = T_Int; imps = [ ("?x", T_Int) ] }) ];
              imps = [];
              ret_typ = T_Int;
              body = E_Num 69;
            };
        ],
        E_App (E_Var "f", E_Num 9) ),
      "fun f (a: int { ?x: int }-> int): int = 69 ; f 9" );
    ( ( [
          D_Fun
            {
              name = "f";
              params =
                [
                  ( "a",
                    T_Func
                      {
                        from = T_Int;
                        to_ = T_UnitTyp;
                        imps =
                          [
                            ("?x", T_Int);
                            ("?y", T_Func { from = T_Int; to_ = T_UnitTyp; imps = [] });
                          ];
                      } );
                ];
              imps = [];
              ret_typ = T_Int;
              body = E_Num 69;
            };
        ],
        E_App (E_Var "f", E_Num 9) ),
      "fun f (a: int { ?x: int, ?y: int -> unit }-> unit): int = 69 ; f 9" );
    (* Functions with implicit parameters declared *)
    ( ( [
          D_Fun
            {
              name = "f";
              params = [ ("a", T_Int); ("b", T_Int) ];
              imps = [ ("?x", T_Int); ("?y", T_Int) ];
              ret_typ = T_Int;
              body = E_ImpVar "?x";
            };
        ],
        E_App (E_Var "f", E_Num 9) ),
      "fun f (a: int) (b: int) { ?x: int, ?y: int } : int = ?x ; f 9" );
    ( ( [
          D_Fun
            {
              name = "f";
              params = [ ("a", T_Int); ("b", T_Int) ];
              imps = [];
              ret_typ = T_Int;
              body = E_Var "a";
            };
        ],
        E_App (E_Var "f", E_Num 9) ),
      "fun f (a: int) (b: int) {}: int = a ; f 9" );
  ]

let test_parse_ast (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let tester () =
    Alcotest.(check implicit_ast)
      (Format.sprintf "Did not produce expected for '%s'" inputs_str)
      expected (parse tokens_seq)
  in
  let name = Format.sprintf "Parser generates valid AST for '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let test_parser_inputs_fail =
  [
    ( "Invalid Syntax. Expected a named binding",
      "fun { ?y: int } (x: int) { ?z: int }: int = x ; f 9" );
    ("Invalid Syntax. Expected ':'", "fun f(x: int) { ?y: int } { ?z: int }: int = x ; f 9");
    ("Invalid Syntax. Expected ':'", "fun f(x: int) { ?y: int } (z: int): int = x ; f 9");
    ( "Invalid Syntax. Expected implicit parameter binding",
      "fun f(x: int) { ?y: int, }: int = x ; f 9" );
    ("Invalid Syntax. Expected a named binding", "fun (x: int): int = x ; f 9");
    ("Invalid Syntax. Requires at least one parameter", "fun f x: int = x ; f 9");
    ("Invalid Syntax. Expected ':'", "fun f (x: int) = x ; f 9");
    ("Invalid Syntax. Expected int, unit or arrow type", "fun f (x: int): blah = x ; f 9");
    ("Invalid Syntax. Requires at least one parameter", "fun f = x ; f 9");
    ("Invalid Syntax. Expected atomic expression", " ; 9 +");
    ("Invalid Syntax. Reached EOF prematurely", " ; ");
    ("Invalid Syntax. Expected a named binding", "val = 9 ; ");
    ("Invalid Syntax. Expected 'fun' or 'val' declaration", "x = 9 ; ");
    ("Invalid Syntax. Expected an implicit binding", " ; letdyn x = 9 in x");
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
  List.map test_parse_ast test_parser_inputs_simple
  @ List.map test_parse_ast test_parser_inputs_application
  @ List.map test_parse_ast test_parser_inputs_val_decls
  @ List.map test_parse_ast test_parser_inputs_fun_decls
  @ List.map test_parse_ast test_parser_inputs_add
  @ List.map test_parse_ast test_parser_inputs_params
  @ List.map test_parse_fail test_parser_inputs_fail
