open Coeffect_Sandbox
open Implicit
open Lexer
open Typecheck
open Types.ET
open Test_core
open Seq

let test_typecheck_inputs_simple =
  [
    (([ { name = "x"; exp = (E_Num 9, T_Int) } ], (E_Var "x", T_Int)), "val x = 9 ; x");
    (([ { name = "x"; exp = (E_Unit, T_Unit) } ], (E_Var "x", T_Unit)), "val x = () ; x");
    (([], (E_Num 9, T_Int)), "; 9");
    (([], (E_Unit, T_Unit)), "; ()");
    (([], (E_Add ((E_Num 6, T_Int), (E_Num 7, T_Int)), T_Int)), "; 6 + 7");
  ]

let test_typecheck_inputs_funcs =
  [
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs { param = ("x", T_Unit); imps = []; body = (E_Num 6, T_Int) },
                T_Func { from = T_Unit; to_ = T_Int; imps = [] } );
          };
        ],
        ( E_App ((E_Var "f", T_Func { from = T_Unit; to_ = T_Int; imps = [] }), (E_Unit, T_Unit)),
          T_Int ) ),
      "fun f(x: unit): unit -> int = 6 ; f ()" );
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs { param = ("x", T_Unit); imps = []; body = (E_Num 6, T_Int) },
                T_Func { from = T_Unit; to_ = T_Int; imps = [] } );
          };
        ],
        ( E_App ((E_Var "f", T_Func { from = T_Unit; to_ = T_Int; imps = [] }), (E_Unit, T_Unit)),
          T_Int ) ),
      "fun f(x: unit) = 6 ; f ()" );
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs
                  {
                    param = ("x", T_Int);
                    imps = [];
                    body =
                      ( E_Abs
                          {
                            param = ("y", T_Int);
                            imps = [];
                            body = (E_Add ((E_Var "x", T_Int), (E_Var "y", T_Int)), T_Int);
                          },
                        T_Func { from = T_Int; to_ = T_Int; imps = [] } );
                  },
                T_Func
                  { from = T_Int; to_ = T_Func { from = T_Int; to_ = T_Int; imps = [] }; imps = [] }
              );
          };
        ],
        ( E_App
            ( ( E_App
                  ( ( E_Var "f",
                      T_Func
                        {
                          from = T_Int;
                          to_ = T_Func { from = T_Int; to_ = T_Int; imps = [] };
                          imps = [];
                        } ),
                    (E_Num 6, T_Int) ),
                T_Func { from = T_Int; to_ = T_Int; imps = [] } ),
              (E_Num 9, T_Int) ),
          T_Int ) ),
      "fun f(x: int) (y: int): int -> int -> int = x + y ; f 6 9" );
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs
                  {
                    param = ("x", T_Int);
                    imps = [];
                    body =
                      ( E_Abs
                          {
                            param = ("y", T_Int);
                            imps = [];
                            body = (E_Add ((E_Var "x", T_Int), (E_Var "y", T_Int)), T_Int);
                          },
                        T_Func { from = T_Int; to_ = T_Int; imps = [] } );
                  },
                T_Func
                  { from = T_Int; to_ = T_Func { from = T_Int; to_ = T_Int; imps = [] }; imps = [] }
              );
          };
        ],
        ( E_App
            ( ( E_App
                  ( ( E_Var "f",
                      T_Func
                        {
                          from = T_Int;
                          to_ = T_Func { from = T_Int; to_ = T_Int; imps = [] };
                          imps = [];
                        } ),
                    (E_Num 6, T_Int) ),
                T_Func { from = T_Int; to_ = T_Int; imps = [] } ),
              (E_Num 9, T_Int) ),
          T_Int ) ),
      "fun f(x: int) (y: int) = x + y ; f 6 9" );
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs
                  { param = ("x", T_Unit); imps = [ ("?y", T_Int) ]; body = (E_ImpVar "?y", T_Int) },
                T_Func { from = T_Unit; to_ = T_Int; imps = [ ("?y", T_Int) ] } );
          };
        ],
        (E_Unit, T_Unit) ),
      "fun f(x: unit) { ?y: int }: unit { ?y: int } -> int = ?y ; ()" );
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs
                  { param = ("x", T_Unit); imps = [ ("?y", T_Int) ]; body = (E_ImpVar "?y", T_Int) },
                T_Func { from = T_Unit; to_ = T_Int; imps = [ ("?y", T_Int) ] } );
          };
        ],
        (E_Unit, T_Unit) ),
      "fun f(x: unit) { ?y: int } = ?y ; ()" );
  ]

let test_typecheck_inputs_letdyn =
  [
    ( ( [
          {
            name = "f";
            exp =
              ( E_Abs
                  {
                    param = ("x", T_Int);
                    imps = [ ("?y", T_Int) ];
                    body = (E_Add ((E_Var "x", T_Int), (E_ImpVar "?y", T_Int)), T_Int);
                  },
                T_Func { from = T_Int; to_ = T_Int; imps = [ ("?y", T_Int) ] } );
          };
        ],
        ( E_LetDyn
            {
              imp = "?y";
              init = (E_Num 9, T_Int);
              body =
                ( E_App
                    ( (E_Var "f", T_Func { from = T_Int; to_ = T_Int; imps = [ ("?y", T_Int) ] }),
                      (E_Num 9, T_Int) ),
                  T_Int );
            },
          T_Int ) ),
      "fun f (x: int) {?y: int}: int { ?y: int } -> int = x + ?y ; letdyn ?y = 9 in f 9" );
  ]

let test_type_check (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let ast = Parser.parse tokens_seq in
  let tester () =
    Alcotest.(check implicit_et)
      (Format.sprintf "Did not produce expected for '%s'" inputs_str)
      expected (type_check ast)
  in
  let name = Format.sprintf "Type checker validates '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let test_parser_inputs_fail =
  [
    (* Basic type checks *)
    ("Unbound variable 'x'", " ; x");
    ("Expected type T_Int, but got T_Unit", " ; () + 9");
    ("Expected app of T_Int -> ...", " ; 6 9");
    ("Expected app of T_Int -> ...", " ; () 9");
    ("Expected type T_Int, but got T_Unit", "fun f (x: int): int -> int = x ; f ()");
    (* Implicit parameters are required *)
    ( "Missing context requirements: { ?y: T_Int }",
      "fun f (x: int) { ?y: int }: int { ?y: int } -> int = x + ?y; f 9" );
    (* Implicit parameters must be bound by letdyn or enclosing function *)
    ("Unbound implicit parameter '?x'", " ; ?x");
    ("Unbound implicit parameter '?y'", "fun f (x: int): int -> int = x + ?y; f");
    ( "Unbound implicit parameter '?z'",
      "fun f (x: int) { ?y: int }: int { ?y: int } -> int = x + ?y + ?z; f" );
    (* Regular variables are not dynamically scoped *)
    ("Unbound variable 'z'", "fun f (x: int): int -> int = x + z\nval z = 8; f");
    (* Type constraints are checked *)
    ("Expected type T_Unit, but got T_Int", "val x: unit = 9 ; ()");
    ( "Expected type T_Func (T_Int -> T_Int), but got T_Func (T_Int -> T_Unit)",
      "fun f (x: int): int -> int = () ; f 8" );
    ( "Expected type T_Func (T_Int -> T_Int), but got T_Func (T_Int -> T_Unit)",
      "fun f (x: int): int -> int = () ; f ()" );
    (* letdyn implicit variables do not linger in other scopes *)
    ( "Missing context requirements: { ?y: T_Int }",
      "fun f (x: int) { ?y: int } = ()\nval x = letdyn ?y = 9 in f 8; f 8" );
    (* binding an already existing implicit parameter with a different type is invalid *)
    ( "Already bound identifier '?x'",
      "fun f (x: int) { ?x: int } = \\(z: int) { ?x: unit } -> () ; ()" );
  ]

let test_typecheck_fail (msg, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let ast = Parser.parse tokens_seq in
  let tester () =
    Alcotest.check_raises "Did not raise exception" (Failure msg) (fun () ->
        let _ = type_check ast in
        ())
  in
  let name = Format.sprintf "Type checker raises exception for '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let suite =
  List.map test_type_check test_typecheck_inputs_simple
  @ List.map test_type_check test_typecheck_inputs_funcs
  @ List.map test_type_check test_typecheck_inputs_letdyn
  @ List.map test_typecheck_fail test_parser_inputs_fail
