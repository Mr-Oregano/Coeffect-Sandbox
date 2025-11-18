open Seq
open Token
open Lexer
open Types

let rec prog_to_string (p : prog) =
  match p with None -> String.empty | Some e -> exp_to_string e

and exp_to_string (e : exp) =
  match e with
  | Abs (v, body) -> Printf.sprintf "Abs (%s) -> (%s)" v (exp_to_string body)
  | App (abs, arg) ->
      Printf.sprintf "App (%s) (%s)" (exp_to_string abs) (exp_to_string arg)
  | Var v -> v
  | Num n -> Int.to_string n

(* TODO: Implement capture-avoiding substitution *)
(* TODO: Rather than immediately raising this, queue this into a list of errors, and attempt recovery *)
let rec parse (ts : token Seq.t) : prog =
  match uncons ts with
  | None -> None
  | Some (t, ts') ->
      let exp, ts' = parse_exp (append (singleton t) ts') in
      (* Assert that the remaining stream is empty! *)
      if not (is_empty ts') then raise (Failure "Invalid Syntax. Expected EOF")
      else Some exp

and parse_exp (ts : token Seq.t) =
  match uncons ts with
  | Some (T_Slash, ts') -> parse_abs ts'
  | Some (t, ts') -> parse_app (append (singleton t) ts')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")

and parse_abs (ts : token Seq.t) =
  let param, ts' = parse_var ts in
  match uncons ts' with
  | Some (T_Period, ts'') ->
      let body, ts''' = parse_exp ts'' in
      (Abs (param, body), ts''')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
  | _ -> raise (Failure "Invalid Syntax. Expected '.'")

and parse_app (ts : token Seq.t) =
  let smp, ts' = parse_simple ts in

  (* The parse_app_tail rule will perform left fold operation to
     reflect left-associativity for the application rule *)
  parse_app_tail smp ts'

and parse_app_tail (curr : exp) (ts : token Seq.t) =
  let smp, ts' = parse_simple_opt ts in
  match smp with
  | Some arg ->
      let new_curr = App (curr, arg) in
      parse_app_tail new_curr ts'
  | None -> (curr, ts')

and parse_simple_opt (ts : token Seq.t) : exp option * token Seq.t =
  match uncons ts with
  | Some (T_Num n, ts') -> (Some (Num n), ts')
  | Some (T_Var v, ts') -> (Some (Var v), ts')
  | Some (T_LParen, ts') ->
      let exp, ts'' = parse_exp ts' in
      (Some exp, consume T_RParen ts'')
  | Some (t, ts') -> (None, append (singleton t) ts')
  | None -> (None, empty)

and parse_simple (ts : token Seq.t) =
  let result, ts' = parse_simple_opt ts in
  match result with
  | Some smp -> (smp, ts')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")

and parse_var (ts : token Seq.t) =
  match uncons ts with
  | Some (T_Var v, ts') -> (v, ts')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
  | _ -> raise (Failure "Invalid Syntax. Expected a named binding")

and consume (t : token) (ts : token Seq.t) =
  match uncons ts with
  | Some (t', ts') ->
      if t' = t then ts'
      else
        raise
          (Failure
             (Printf.sprintf "Invalid Syntax. Expected '%s'" (token_to_string t)))
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
