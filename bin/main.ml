open Coeffect_Sandbox
open Extchannel
open Printf
open Token

let interpret interpreter (in_ : in_channel) =
  let tokens = Lexer.lex (In_channel.to_seq in_) in
  interpreter tokens

let interpret_lambda (tokens : token Seq.t) =
  let prog = Lambda.Parser.parse tokens in
  let res = Lambda.Interpreter.eval prog in
  Lambda.Interpreter.res_to_string res

let interpret_implicit (tokens : token Seq.t) =
  let ast = Implicit.Parser.parse tokens in
  let prog = Implicit.Typecheck.type_check ast in
  let res = Implicit.Interpreter.eval prog in
  Implicit.Interpreter.res_to_string res

let usage () = sprintf "Usage: %s <FILE> [-l (lambda|implicit)]" Sys.argv.(0) |> prerr_endline

let usage_and_exit () =
  usage ();
  exit (-1)

(* Driver *)
let _ =
  let argc = Array.length Sys.argv in
  if argc < 2 then usage_and_exit ()
  else
    let language_mode_flag = Array.find_index (fun s -> s = "-l") Sys.argv in
    let interpreter =
      match language_mode_flag with
      | None -> interpret_implicit
      | Some i -> (
          if argc < i + 2 then usage_and_exit ()
          else
            match Sys.argv.(i + 1) with
            | "lambda" -> interpret_lambda
            | "implicit" -> interpret_implicit
            | _ -> usage_and_exit ())
    in
    let result = In_channel.with_open_text Sys.argv.(1) (interpret interpreter) in
    print_endline result
