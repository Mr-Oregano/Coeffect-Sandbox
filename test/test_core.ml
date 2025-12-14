open Coeffect_Sandbox
open Alcotest
open Format

let pp_token fmt tk = fprintf fmt "%s" (Lexer.token_to_string tk)
let token = testable pp_token ( = )
let seq_token = seq token

(* Lambda Calculus *)
let lambda_pp_prog fmt p = fprintf fmt "%s" (Lambda.Parser.prog_to_string p)
let lambda_prog = testable lambda_pp_prog ( = )
let lambda_pp_res fmt r = fprintf fmt "%s" (Lambda.Interpreter.res_to_string r)
let lambda_res = testable lambda_pp_res ( = )

(* Implicit Parameters *)
let implicit_pp_ast fmt p = fprintf fmt "%s" (Implicit.Parser.prog_to_string p)
let implicit_ast = testable implicit_pp_ast ( = )

let implicit_pp_ctx fmt ((id, typ) : Implicit.Types.ET.id * Implicit.Types.ET.typ) =
  fprintf fmt "%s: %s" id (Implicit.Typecheck.typ_to_string typ)

let implicit_ctx = testable implicit_pp_ctx ( = )
let implicit_seq_ctx = seq implicit_ctx
let implicit_pp_typ fmt typ = fprintf fmt "%s" (Implicit.Typecheck.typ_to_string typ)
let implicit_typ = testable implicit_pp_typ ( = )
let implicit_pp_et fmt p = fprintf fmt "%s" (Implicit.Typecheck.prog_to_string p)
let implicit_et = testable implicit_pp_et ( = )

let implicit_pp_bind fmt ((id, v) : Implicit.Types.Interp.binding) =
  fprintf fmt "%s: %s" id (Implicit.Interpreter.value_to_string v)

let implicit_bind = testable implicit_pp_bind ( = )
let implicit_seq_bind = list implicit_bind
let implicit_pp_val fmt v = fprintf fmt "%s" (Implicit.Interpreter.value_to_string v)
let implicit_val = testable implicit_pp_val ( = )
let implicit_res = implicit_val
