open Coeffect_Sandbox

(* open Implicit.Types.Interp *)
open Implicit.Environments
open Test_core
open Env
open Option
open List

let test_environment_empty () =
  let env = Env.empty () in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not generate empty vars")
    [] (get_vars env);
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not generate empty imps")
    [] (get_imps env)

let test_environment_add () =
  let env = Env.empty () in
  let () = add_var env ("x", V_Num 69) in
  let () = add_imp env ("?x", V_Num 420) in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not add vars")
    [ ("x", V_Num 69) ]
    (get_vars env);
  Alcotest.(check implicit_val)
    (Format.sprintf "Environment does not add vars")
    (V_Num 69)
    (get (get_var env "x"));
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not add imps")
    [ ("?x", V_Num 420) ]
    (get_imps env);
  Alcotest.(check implicit_val)
    (Format.sprintf "Environment does not add vars")
    (V_Num 420)
    (get (get_imp env "?x"))

let test_environment_add_multiple () =
  let env = Env.empty () in
  let () = add_vars env [ ("x", V_Num 69) ] in
  let () = add_imps env [ ("?x", V_Num 420) ] in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not add vars")
    [ ("x", V_Num 69) ]
    (get_vars env);
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not add imps")
    [ ("?x", V_Num 420) ]
    (get_imps env)

let test_environment_add_shadow () =
  let env = Env.empty () in
  let () = add_var env ("x", V_Num 69) in
  let () = add_imp env ("?x", V_Num 420) in
  (* Shadow *)
  let () = add_var_shadow env ("x", V_Num 0xCAFEBABE) in
  let () = add_imp_shadow env ("?x", V_Num 0xDEADBEEF) in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow vars")
    [ ("x", V_Num 0xCAFEBABE) ]
    (get_vars env);
  Alcotest.(check implicit_val)
    (Format.sprintf "Environment does not shadow vars")
    (V_Num 0xCAFEBABE)
    (get (get_var env "x"));
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow imps")
    [ ("?x", V_Num 0xDEADBEEF) ]
    (get_imps env);
  Alcotest.(check implicit_val)
    (Format.sprintf "Environment does not shadow vars")
    (V_Num 0xDEADBEEF)
    (get (get_imp env "?x"));
  let () = remove_var env "x" in
  let () = remove_imp env "?x" in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow vars")
    [ ("x", V_Num 69) ]
    (get_vars env);
  Alcotest.(check implicit_val)
    (Format.sprintf "Environment does not shadow vars")
    (V_Num 69)
    (get (get_var env "x"));
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow imps")
    [ ("?x", V_Num 420) ]
    (get_imps env);
  Alcotest.(check implicit_val)
    (Format.sprintf "Environment does not shadow vars")
    (V_Num 420)
    (get (get_imp env "?x"))

let test_environment_shadow_multiple () =
  let env = Env.empty () in
  let () = add_vars env [ ("x", V_Num 69) ] in
  let () = add_imps env [ ("?x", V_Num 420) ] in
  (* Shadow *)
  let () = add_vars_shadow env [ ("x", V_Num 0xCAFEBABE) ] in
  let () = add_imps_shadow env [ ("?x", V_Num 0xDEADBEEF) ] in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow vars")
    [ ("x", V_Num 0xCAFEBABE) ]
    (get_vars env);
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow imps")
    [ ("?x", V_Num 0xDEADBEEF) ]
    (get_imps env);
  let () = remove_var env "x" in
  let () = remove_imp env "?x" in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow vars")
    [ ("x", V_Num 69) ]
    (get_vars env);
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not shadow imps")
    [ ("?x", V_Num 420) ]
    (get_imps env)

let test_environment_remove () =
  let env = Env.empty () in
  let () = add_var env ("x", V_Num 69) in
  let () = add_imp env ("?x", V_Num 420) in
  let () = remove_var env "x" in
  let () = remove_imp env "?x" in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not remove vars")
    [] (get_vars env);
  Alcotest.(check bool)
    (Format.sprintf "Environment does not remove vars")
    true
    (is_none (get_var env "x"));
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not remove imps")
    [] (get_imps env);
  Alcotest.(check bool)
    (Format.sprintf "Environment does not remove vars")
    true
    (is_none (get_imp env "?x"))

let test_environment_remove_multiple () =
  let env = Env.empty () in
  let () = add_vars env [ ("x", V_Num 69); ("y", V_Unit); ("z", V_Num 666) ] in
  let () = add_imps env [ ("?x", V_Num 420); ("?y", V_Unit); ("?z", V_Num 999) ] in
  let () = remove_vars env [ "y"; "z" ] in
  let () = remove_imps env [ "?y"; "?z" ] in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not remove vars")
    [ ("x", V_Num 69) ]
    (get_vars env);
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not remove imps")
    [ ("?x", V_Num 420) ]
    (get_imps env)

let test_environment_get_imps_excluded () =
  let env = Env.empty () in
  let () = add_imps env [ ("?x", V_Num 420); ("?y", V_Unit); ("?z", V_Num 999) ] in
  Alcotest.(check implicit_seq_bind)
    (Format.sprintf "Environment does not get non-excluded imps")
    [ ("?x", V_Num 420) ]
    (get_imps_excluded env (to_seq [ "?y"; "?z" ]))

let suite =
  [
    Alcotest.test_case "Env generates empty" `Quick test_environment_empty;
    Alcotest.test_case "Env adds entries" `Quick test_environment_add;
    Alcotest.test_case "Env adds multiple entries" `Quick test_environment_add_multiple;
    Alcotest.test_case "Env shadows entries" `Quick test_environment_add_shadow;
    Alcotest.test_case "Env shadows multiple entries" `Quick test_environment_shadow_multiple;
    Alcotest.test_case "Env removes entries" `Quick test_environment_remove;
    Alcotest.test_case "Env removes multiple entries" `Quick test_environment_remove_multiple;
    Alcotest.test_case "Env gets imps with exclusions" `Quick test_environment_get_imps_excluded;
  ]
