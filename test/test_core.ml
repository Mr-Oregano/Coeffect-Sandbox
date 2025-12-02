open Coeffect_Sandbox
open Alcotest
open Format

let pp_token fmt tk = fprintf fmt "%s" (Lexer.token_to_string tk)
let token = testable pp_token (fun t1 t2 -> t1 = t2)
let seq_token = seq token

(* Lambda Calculus *)
let lambda_pp_prog fmt p = fprintf fmt "%s" (Lambda.Parser.prog_to_string p)
let lambda_prog = testable lambda_pp_prog (fun p1 p2 -> p1 = p2)
let lambda_pp_res fmt r = fprintf fmt "%s" (Lambda.Interpreter.res_to_string r)
let lambda_res = testable lambda_pp_res (fun r1 r2 -> r1 = r2)

(* Implicit Parameters *)
let implicit_pp_ast fmt p = fprintf fmt "%s" (Implicit.Parser.prog_to_string p)
let implicit_ast = testable implicit_pp_ast (fun p1 p2 -> p1 = p2)

let implicit_pp_env fmt
    ((id, typ) : Implicit.Types.ET.id * Implicit.Types.ET.typ) =
  fprintf fmt "%s: %s" id (Implicit.Typecheck.typ_to_string typ)

let implicit_env = testable implicit_pp_env (fun (id1, _) (id2, _) -> id1 = id2)
let implicit_seq_env = seq implicit_env

let implicit_pp_typ fmt typ =
  fprintf fmt "%s" (Implicit.Typecheck.typ_to_string typ)

let implicit_typ = testable implicit_pp_typ (fun typ1 typ2 -> typ1 = typ2)

let implicit_pp_et fmt p =
  fprintf fmt "%s" (Implicit.Typecheck.prog_to_string p)

let implicit_et = testable implicit_pp_et (fun p1 p2 -> p1 = p2)
