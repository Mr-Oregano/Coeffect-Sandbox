open Coeffect_Sandbox
open Implicit.Types.ET
open Implicit.Environments
open Test_core
open Context
open Option
open Seq

let test_context_empty () =
  let ctx = Context.empty () in
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not generate empty vars")
    empty (get_vars ctx);
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not generate empty imps")
    empty (get_imps ctx)

let test_context_add () =
  let ctx = Context.empty () in
  let () = add_var ctx ("x", T_Int) in
  let () = add_imp ctx ("?x", T_Int) in
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not add vars")
    (singleton ("x", T_Int))
    (get_vars ctx);
  Alcotest.(check implicit_typ)
    (Format.sprintf "Context does not add vars")
    T_Int
    (get (get_var ctx "x"));
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not add imps")
    (singleton ("?x", T_Int))
    (get_imps ctx);
  Alcotest.(check implicit_typ)
    (Format.sprintf "Context does not add vars")
    T_Int
    (get (get_imp ctx "?x"))

let test_context_add_multiple () =
  let ctx = Context.empty () in
  let () = add_vars ctx [ ("x", T_Int) ] in
  let () = add_imps ctx [ ("?x", T_Int) ] in
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not add vars")
    (List.to_seq [ ("x", T_Int) ])
    (get_vars ctx);
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not add imps")
    (List.to_seq [ ("?x", T_Int) ])
    (get_imps ctx)

let test_context_remove () =
  let ctx = Context.empty () in
  let () = add_var ctx ("x", T_Int) in
  let () = add_imp ctx ("?x", T_Int) in
  let () = remove_var ctx "x" in
  let () = remove_imp ctx "?x" in
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not remove vars")
    empty (get_vars ctx);
  Alcotest.(check bool)
    (Format.sprintf "Context does not remove vars")
    true
    (is_none (get_var ctx "x"));
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not remove imps")
    empty (get_imps ctx);
  Alcotest.(check bool)
    (Format.sprintf "Context does not remove vars")
    true
    (is_none (get_imp ctx "?x"))

let test_context_remove_multiple () =
  let ctx = Context.empty () in
  let () = add_vars ctx [ ("x", T_Int); ("y", T_Int); ("z", T_Int) ] in
  let () = add_imps ctx [ ("?x", T_Int); ("?y", T_Int); ("?z", T_Int) ] in
  let () = remove_vars ctx [ "y"; "z" ] in
  let () = remove_imps ctx [ "?y"; "?z" ] in
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not remove vars")
    (List.to_seq [ ("x", T_Int) ])
    (get_vars ctx);
  Alcotest.(check implicit_seq_ctx)
    (Format.sprintf "Context does not remove imps")
    (List.to_seq [ ("?x", T_Int) ])
    (get_imps ctx)

let suite =
  [
    Alcotest.test_case "Context generates empty" `Quick test_context_empty;
    Alcotest.test_case "Context adds entries" `Quick test_context_add;
    Alcotest.test_case "Context adds multiple entries" `Quick test_context_add_multiple;
    Alcotest.test_case "Context removes entries" `Quick test_context_remove;
    Alcotest.test_case "Context removes multiple entries" `Quick test_context_remove_multiple;
  ]
