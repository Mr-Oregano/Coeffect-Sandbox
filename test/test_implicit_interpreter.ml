open List
open Seq
open Coeffect_Sandbox
open Lexer
open Implicit.Parser
open Implicit.Interpreter
open Implicit.Typecheck
open Implicit.Types.Interp
open Implicit.Types.ET
open Test_core

let test_interpret_inputs_simple =
  [
    (V_Num 69, " ; 69");
    (V_Num 69, "val x = 69 ; x");
    (V_Num 489, "val x = 69\nval y = 420 ; x + y");
    (V_Num 2, " ; 1 + 1");
    (V_Num 69, "fun f (x: int): int -> int = x ; f 69");
  ]

(* TODO: Figure out a way to test lists without caring about order *)
let test_interpret_inputs_curry =
  [
    (V_Num 489, "fun f (x: int) (y: int): int -> int -> int = x + y; f 69 420");
    ( V_Clo
        {
          param_id = "y";
          env = [ ("x", V_Num 69) ];
          imps = [];
          body = (E_Add ((E_Var "x", T_Int), (E_Var "y", T_Int)), T_Int);
        },
      "fun f (x: int) (y: int): int -> int -> int = x + y; f 69" );
    ( V_Clo
        {
          param_id = "x";
          env = [];
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
      "fun f (x: int) (y: int): int -> int -> int = x + y; f" );
  ]

let test_interpret_inputs_imps =
  [
    ( V_Num 489,
      "fun f (x: int) { ?y: int }: int { ?y: int } -> int = x + ?y; letdyn ?y = 420 in f 69" );
    ( V_Clo
        {
          param_id = "y";
          env = [ ("x", V_Num 1) ];
          imps = [];
          body =
            ( E_Add ((E_Add ((E_Var "x", T_Int), (E_Var "y", T_Int)), T_Int), (E_ImpVar "?z", T_Int)),
              T_Int );
        },
      "fun f (x: int) (y: int) { ?z: int }: int -> int { ?z: int } -> int = x + y + ?z; f 1" );
  ]

let test_interpret_inputs_misc =
  [
    (* Implicit variables are statically scoped first, then dynamically scoped *)
    ( V_Num 3,
      "fun baz (y: int) {?x: int}\n\
      \   : int { ?x: int } -> int -> int\n\
      \   = \\(z: int) -> ?x + y + z\n\n\
       val new2 \n\
      \   : int -> int\n\
      \   = letdyn ?x = 1 in baz 1\n\
       ;\n\
       letdyn ?x = 0 in new2 1" );
    ( V_Num 3,
      "fun baz (y: int) {?x: int}\n\
      \   : int { ?x: int } -> int -> int\n\
      \   = \\(z: int) -> ?x + y + z\n\n\
       val new2 \n\
      \   : int -> int\n\
      \   = letdyn ?x = 1 in baz 1\n\
       ;\n\
       new2 1" );
    (* Programmer can request explicit dynamic scoping *)
    ( V_Num 3,
      "fun bam (y: int) { ?x: int }\n\
      \   : int { ?x: int } -> int { ?x: int } -> int\n\
      \   = (\\(x: int) -> \\(z: int) { ?x: int } -> x + ?x + y + z) ?x\n\n\
       val new3\n\
      \   : int { ?x: int } -> int\n\
      \   = letdyn ?x = 1 in bam 1\n\
       ;\n\
       letdyn ?x = 0 in new3 1" );
  ]

let test_interpret_prog (expected, inputs) =
  let inputs_str = String.escaped inputs in
  let chars_seq = once (String.to_seq inputs) in
  let tokens_seq = lex chars_seq in
  let ast = parse tokens_seq in
  let prog = type_check ast in
  let tester () =
    Alcotest.(check implicit_res)
      (Format.sprintf "Did not produce expected for '%s'" inputs_str)
      expected (eval prog)
  in
  let name = Format.sprintf "Interp resolves to valid value for '%s'" inputs_str in
  Alcotest.test_case name `Quick tester

let suite =
  List.map test_interpret_prog test_interpret_inputs_simple
  @ List.map test_interpret_prog test_interpret_inputs_curry
  @ List.map test_interpret_prog test_interpret_inputs_imps
  @ List.map test_interpret_prog test_interpret_inputs_misc
