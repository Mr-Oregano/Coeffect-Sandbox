(* Very very simple tree-walk lambda calculus interpreter *)
open Parser
open Types

let rec res_to_string (r : res) =
  match r with None -> "<NO-OP>" | Some v -> value_to_string v

and value_to_string (v : value) =
  match v with
  | Num n -> Int.to_string n
  | Clo (v, e, c) ->
      Printf.sprintf "(%s) |- \\%s. %s" (ctx_to_string c) v (exp_to_string e)

and ctx_to_string (c : ctx) =
  String.concat "; "
    (List.map
       (fun (name, value) ->
         Printf.sprintf "%s: %s" name (value_to_string value))
       c)

let rec eval (p : prog) : res =
  match p with None -> None | Some exp -> Some (eval_expr exp [])

and eval_expr (e : exp) (ctx : ctx) =
  match e with
  | Num v -> Num v
  | Var v ->
      let _, vl = List.find (fun (vr, _) -> vr = v) ctx in
      vl
  | Abs (var, f_exp) -> Clo (var, f_exp, ctx)
  | App (f_exp, arg_exp) -> (
      let f = eval_expr f_exp ctx in
      let arg = eval_expr arg_exp ctx in
      match f with
      | Clo (vr, body, env) -> eval_expr body ((vr, arg) :: env)
      | _ -> raise (Failure "Runtime Error. Cannot apply to non function-type"))
