open Seq
open Token
open Types.Ast
open Printf

let rec prog_to_string (p : prog) =
  let decls, exp = p in
  let decl_merge acc d = acc ^ " " ^ decl_to_string d in
  let decls_str = List.fold_left decl_merge "" decls in
  decls_str ^ " ; " ^ exp_to_string exp

and decl_to_string (d : decl) =
  let { name; exp; typ_opt } = d in
  let s = sprintf "%s" name in
  let s' = match typ_opt with None -> s | Some t -> sprintf "%s: %s" s (typ_to_string t) in
  sprintf "%s = %s" s' (exp_to_string exp)

and typ_to_string (t : typ) =
  match t with
  | T_Int -> "T_Int"
  | T_Unit -> "T_Unit"
  | T_Func { from; to_; imps } ->
      let s = sprintf "%s" (typ_to_string from) in
      let s' = match imps with [] -> s | _ -> sprintf "%s {%s}" s (imps_to_string imps) in
      sprintf "T_Func (%s -> %s)" s' (typ_to_string to_)

and imp_to_string ((id, typ) : id * typ) = sprintf "%s: %s" id (typ_to_string typ)

and imps_to_string (ls : (id * typ) list) =
  let strings = List.map imp_to_string ls in
  String.concat ", " strings

and param_to_string ((id, typ) : param) = sprintf "(%s: %s)" id (typ_to_string typ)

and exp_to_string (e : exp) =
  match e with
  | E_Abs { param; imps; body } ->
      let s = sprintf "E_Abs \\%s" (param_to_string param) in
      let s' = match imps with [] -> s | _ -> sprintf "%s {%s}" s (imps_to_string imps) in
      sprintf "%s -> (%s)" s' (exp_to_string body)
  | E_App (abs, arg) -> sprintf "E_App (%s) (%s)" (exp_to_string abs) (exp_to_string arg)
  | E_Add (a, b) -> sprintf "E_Add (%s) (%s)" (exp_to_string a) (exp_to_string b)
  | E_Var id -> sprintf "E_Var (%s)" id
  | E_ImpVar id -> sprintf "E_ImpVar (%s)" id
  | E_Unit -> "E_Unit"
  | E_Num i -> sprintf "E_Num (%s)" (Int.to_string i)
  | E_LetDyn { imp; init; body } ->
      sprintf "E_LetDyn %s = (%s) in (%s)" imp (exp_to_string init) (exp_to_string body)

(* TODO: Rather than immediately raising errors, queue them into a list, and attempt recovery *)
(* TODO: Use monadic bind ( "let*" ) or Stream construct to avoid the excessive use of ts' references! *)
(* TODO: Consider using a better structure than an immutable list for lists of AST nodes *)
(* TODO: Error messages should be more descriptive and dynamically adjust to the structure of the AST *)
let rec parse (ts : token Seq.t) : prog =
  match uncons ts with
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
  | Some (t, ts') ->
      let decls, ts'' = parse_decls (append (singleton t) ts') in
      let exp, ts''' = parse_exp ts'' in
      (* Assert that the remaining stream is empty! *)
      if not (is_empty ts''') then raise (Failure "Invalid Syntax. Expected EOF") else (decls, exp)

and parse_decls (ts : token Seq.t) =
  let rec _aux decls ts' =
    match uncons ts' with
    | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
    | Some (TK_Semicolon, ts'') -> (decls, ts'')
    | Some (t, ts'') ->
        let decl, ts''' = parse_decl (append (singleton t) ts'') in
        _aux (decl :: decls) ts'''
  in
  let decls, ts' = _aux [] ts in
  (* We reverse the list of declarations so they appear in lexical order *)
  (List.rev decls, ts')

and unroll_curried (params : param list) (imps : imp list) (body : exp) =
  let _aux param (exp, imps) = (E_Abs { param; imps; body = exp }, []) in
  fst (List.fold_right _aux params (body, imps))

and parse_decl (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_KW_Val, ts') ->
      let id, ts'' = parse_id ts' in
      let ts'' = consume TK_Equals ts'' in
      let v, ts'' = parse_exp ts'' in
      ({ name = id; exp = v; typ_opt = None }, ts'')
  | Some (TK_KW_Fun, ts') ->
      let id, ts'' = parse_id ts' in
      let params, ts'' = parse_params ts'' in
      if List.is_empty params then raise (Failure "Invalid Syntax. Requires at least one parameter")
      else
        let imps_opt, ts'' = parse_imps_opt ts'' in
        let imps = Option.value imps_opt ~default:[] in
        let ts'' = consume TK_Colon ts'' in
        let typ, ts'' = parse_type ts'' in
        let ts'' = consume TK_Equals ts'' in
        let body, ts'' = parse_exp ts'' in
        let exp = unroll_curried params imps body in
        ({ name = id; exp; typ_opt = Some typ }, ts'')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
  | _ -> raise (Failure "Invalid Syntax. Expected 'fun' or 'val' declaration")

and parse_params (ts : token Seq.t) =
  let rec _aux params ts' =
    match uncons ts' with
    | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
    | Some ((TK_LParen as t), ts'') ->
        let param, ts''' = parse_param (append (singleton t) ts'') in
        _aux (param :: params) ts'''
    | Some (t, ts'') -> (params, append (singleton t) ts'')
  in
  let params, ts' = _aux [] ts in
  (* We reverse the list of parameters so they appear in lexical order *)
  (List.rev params, ts')

and parse_param (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_LParen, ts') ->
      let id, ts' = parse_id ts' in
      let ts' = consume TK_Colon ts' in
      let typ, ts' = parse_type ts' in
      let ts' = consume TK_RParen ts' in
      ((id, typ), ts')
  | Some _ -> raise (Failure "Invalid Syntax. Expected parameter binding")
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")

and parse_type (ts : token Seq.t) =
  let base_typ, ts' = parse_base_type ts in
  let imps_opt, ts'' = parse_imps_opt ts' in
  match uncons ts'' with
  | Some (TK_Arrow, ts''') ->
      let imps = Option.value imps_opt ~default:[] in
      let ret_typ, ts'''' = parse_type ts''' in
      (T_Func { from = base_typ; to_ = ret_typ; imps }, ts'''')
  | Some (t, ts''') -> (base_typ, append (singleton t) ts''')
  | None -> (base_typ, ts'')

and parse_imps_opt (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_LCurly, ts') ->
      let params, ts'' = parse_imps_inner ts' in
      let ts''' = consume TK_RCurly ts'' in
      (Some params, ts''')
  | Some (t, ts') -> (None, append (singleton t) ts')
  | None -> (None, empty)

and parse_imps_inner (ts : token Seq.t) =
  let rec _aux imps ts' =
    match uncons ts' with
    | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
    | Some (TK_Comma, ts'') ->
        let imp, ts''' = parse_imp ts'' in
        _aux (imp :: imps) ts'''
    | Some (t, ts'') -> (imps, append (singleton t) ts'')
  in
  let first_opt, ts' = parse_imp_opt ts in
  let acc = match first_opt with Some imp -> [ imp ] | _ -> [] in
  let imps, ts' = _aux acc ts' in
  (* We reverse the list of parameters so they appear in lexical order *)
  (List.rev imps, ts')

and parse_imp_opt (ts : token Seq.t) =
  let id_opt, ts' = parse_imp_id_opt ts in
  match id_opt with
  | Some id ->
      let ts' = consume TK_Colon ts' in
      let typ, ts' = parse_type ts' in
      (Some (id, typ), ts')
  | None -> (None, ts')

and parse_imp (ts : token Seq.t) =
  match parse_imp_opt ts with
  | Some imp, ts' -> (imp, ts')
  | _ -> raise (Failure "Invalid Syntax. Expected implicit parameter binding")

and parse_base_type (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_KW_Int, ts') -> (T_Int, ts')
  | Some (TK_KW_Unit, ts') -> (T_Unit, ts')
  | Some (TK_LParen, ts') ->
      let typ, ts'' = parse_type ts' in
      let ts''' = consume TK_RParen ts'' in
      (typ, ts''')
  | Some _ -> raise (Failure "Invalid Syntax. Expected int, unit or arrow type")
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")

and parse_exp (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_KW_LetDyn, ts') -> parse_letdyn (append (singleton TK_KW_LetDyn) ts')
  | Some (t, ts') -> parse_add (append (singleton t) ts')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")

and parse_letdyn (ts : token Seq.t) =
  let ts' = consume TK_KW_LetDyn ts in
  let imp_id, ts' = parse_imp_id ts' in
  let ts' = consume TK_Equals ts' in
  let init, ts' = parse_exp ts' in
  let ts' = consume TK_KW_In ts' in
  let body, ts' = parse_exp ts' in
  (E_LetDyn { imp = imp_id; init; body }, ts')

and parse_add (ts : token Seq.t) =
  let app, ts' = parse_app ts in

  (* The parse_add_tail rule will perform left fold operation to
     reflect left-associativity for the addition rule *)
  parse_add_tail app ts'

and parse_add_tail (curr : exp) (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_Plus, ts') ->
      let app, ts'' = parse_app ts' in
      let new_curr = E_Add (curr, app) in
      parse_add_tail new_curr ts''
  | Some (t, ts') -> (curr, append (singleton t) ts')
  | None -> (curr, ts)

and parse_app (ts : token Seq.t) =
  let smp, ts' = parse_simple ts in

  (* The parse_app_tail rule will perform left fold operation to
     reflect left-associativity for the application rule *)
  parse_app_tail smp ts'

and parse_app_tail (curr : exp) (ts : token Seq.t) =
  let smp, ts' = parse_simple_opt ts in
  match smp with
  | Some arg ->
      let new_curr = E_App (curr, arg) in
      parse_app_tail new_curr ts'
  | None -> (curr, ts')

and parse_simple_opt (ts : token Seq.t) : exp option * token Seq.t =
  match uncons ts with
  | Some (TK_Num n, ts') -> (Some (E_Num n), ts')
  | Some (TK_Var v, ts') -> (Some (E_Var v), ts')
  | Some (TK_ImpVar v, ts') -> (Some (E_ImpVar v), ts')
  | Some (TK_Unit, ts') -> (Some E_Unit, ts')
  | Some (TK_LParen, ts') ->
      let exp, ts'' = parse_exp ts' in
      (Some exp, consume TK_RParen ts'')
  | Some (t, ts') -> (None, append (singleton t) ts')
  | None -> (None, empty)

and parse_simple (ts : token Seq.t) =
  let result, ts' = parse_simple_opt ts in
  match result with
  | Some smp -> (smp, ts')
  | None -> raise (Failure "Invalid Syntax. Expected atomic expression")

and parse_imp_id_opt (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_ImpVar v, ts') -> (Some v, ts')
  | Some (t, ts') -> (None, append (singleton t) ts')
  | None -> (None, empty)

and parse_imp_id (ts : token Seq.t) =
  match parse_imp_id_opt ts with
  | Some id, ts' -> (id, ts')
  | _ -> raise (Failure "Invalid Syntax. Expected an implicit binding")

and parse_id (ts : token Seq.t) =
  match uncons ts with
  | Some (TK_Var v, ts') -> (v, ts')
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
  | _ -> raise (Failure "Invalid Syntax. Expected a named binding")

and consume (t : token) (ts : token Seq.t) =
  match uncons ts with
  | Some (t', ts') ->
      if t' = t then ts'
      else raise (Failure (sprintf "Invalid Syntax. Expected '%s'" (Lexer.token_to_string t)))
  | None -> raise (Failure "Invalid Syntax. Reached EOF prematurely")
