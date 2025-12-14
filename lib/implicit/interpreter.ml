open Types.Interp
open Types
open Environments
open Environments.Env
open Printf

let rec res_to_string (r : res) = value_to_string r

and value_to_string (v : value) =
  match v with
  | V_Unit -> "V_Unit"
  | V_Num i -> sprintf "V_Num %s" (Int.to_string i)
  | V_Clo c -> clo_to_string c

and clo_to_string (c : clo) =
  sprintf "V_Clo \\%s (env: [%s], imps: {%s}) -> (%s)" c.param_id (bindings_to_string c.env)
    (bindings_to_string c.imps) (Typecheck.exp_to_string c.body)

(* TODO: Figure out a way to pretty-print the values, these might be needed for tests *)
and bindings_to_string ?(delim = ", ") (bs : binding list) =
  let strings = List.map fst bs in
  String.concat delim strings

let rec eval (p : ET.prog) : res =
  let env = empty () in
  let decls, exp = p in
  let () = List.iter (eval_decl env) decls in
  eval_exp env exp

and eval_decl (env : Env.t) (d : ET.decl) =
  let v = eval_exp env d.exp in
  add_var env (d.name, v)

and eval_exp (env : Env.t) (e : ET.exp) =
  match e with
  | ET.E_Abs { param; body; imps }, _ ->
      (* This is the Abs semantics from the notes *)
      (* Abs captures the values of variables in lexical scope at declaration time *)
      let lex_env = get_vars env in
      (* NOTE: In this model, the programmer explicitly states what they want dynamically bound
         it's 'imps'! Everything else is statically bound. This means our closures will also
         track all the imps that do not appear in this user-provided list *)
      let lex_imps = get_imps_excluded env (Seq.map fst (List.to_seq imps)) in
      V_Clo { param_id = fst param; body; env = lex_env; imps = lex_imps }
  | ET.E_App (func, arg), _ ->
      (* This is the App semantics from the notes *)
      (* App evaluates the function, followed by the argument *)
      (* Calling the function with the argument involves adding the function 
         param with the value of the arg to the env and then evaluating the body *)
      let { param_id; body; env = lex_env; imps = lex_imps } = extract_clo (eval_exp env func) in
      let arg' = eval_exp env arg in
      (* NOTE: We create a brand new empty environment here (we don't want to be using
               bindings from our call-site environment! We do this with the controlled 
               implicit parameters) *)
      let env' = Env.empty () in
      let () = add_var env' (param_id, arg') in
      let () = add_vars env' lex_env in
      (* Here is where we add the implicit parameters, add the dynamic ones first
         then shadow them with the ones that were lexically scoped *)
      let () = add_imps env' (get_imps env) in
      let () = add_imps_shadow env' lex_imps in
      eval_exp env' body
  | ET.E_Add (e1, e2), _ ->
      let v1 = extract_num (eval_exp env e1) in
      let v2 = extract_num (eval_exp env e2) in
      V_Num (v1 + v2)
  | ET.E_Var name, _ ->
      (* Just get option, since we type-checked *)
      Option.get (get_var env name)
  | ET.E_ImpVar name, _ ->
      (* This is the Param semantics from the notes *)
      Option.get (get_imp env name)
  | ET.E_Unit, _ -> V_Unit
  | ET.E_Num n, _ -> V_Num n
  | ET.E_LetDyn { imp; init; body }, _ ->
      (* This is the LetDyn semantics from the notes *)
      (* LetDyn binds a new instance of this implicit parameter (fully replacing any existing entry) *)
      let v = eval_exp env init in
      let () = add_imp env (imp, v) in
      eval_exp env body

and extract_num (v : value) =
  match v with
  | V_Num n -> n
  | _ ->
      raise
        (Failure (sprintf "Runtime Error. Attempted to extract num from %s" (value_to_string v)))

and extract_clo (v : value) =
  match v with
  | V_Clo c -> c
  | _ ->
      raise
        (Failure (sprintf "Runtime Error. Attempted to extract closure from %s" (value_to_string v)))
