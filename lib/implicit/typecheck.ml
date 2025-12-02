open Types
open Environments
open Printf
open Context
open List
open Option

let rec prog_to_string (p : ET.prog) =
  let decls, exp = p in
  let decl_merge acc d = acc ^ " " ^ decl_to_string d in
  let decls_str = List.fold_left decl_merge "" decls in
  decls_str ^ " ; " ^ exp_to_string exp

and decl_to_string (d : ET.decl) =
  match d with
  | ET.D_Val (id, exp) -> Printf.sprintf "D_Val %s = (%s)" id (exp_to_string exp)
  | ET.D_Fun { name; params; ret_typ; body; imps } ->
      let s = Printf.sprintf "D_Fun %s %s" name (params_to_string params) in
      let s' = match imps with [] -> s | _ -> Printf.sprintf "%s {%s}" s (imps_to_string imps) in
      Printf.sprintf "%s: %s = (%s)" s' (typ_to_string ret_typ) (exp_to_string body)

and typ_to_string (t : ET.typ) : string =
  match t with
  | ET.T_Int -> "T_Int"
  | ET.T_Unit -> "T_Unit"
  | ET.T_Func { from; to_; imps } ->
      let s = Printf.sprintf "%s" (typ_to_string from) in
      let s' = match imps with [] -> s | _ -> Printf.sprintf "%s {%s}" s (imps_to_string imps) in
      Printf.sprintf "T_Func (%s -> %s)" s' (typ_to_string to_)

and imp_to_string ((id, typ) : ET.imp) = Printf.sprintf "%s: %s" id (typ_to_string typ)

and imps_to_string (ls : ET.imp list) =
  let strings = List.map imp_to_string ls in
  String.concat ", " strings

and param_to_string ((id, typ) : ET.param) = Printf.sprintf "(%s: %s)" id (typ_to_string typ)

and params_to_string (ps : ET.param list) =
  let strings = List.map param_to_string ps in
  String.concat " " strings

and exp_to_string (e : ET.exp) =
  match e with
  | ET.E_App (abs, arg), typ ->
      Printf.sprintf "E_App (%s) (%s): %s" (exp_to_string abs) (exp_to_string arg)
        (typ_to_string typ)
  | ET.E_Add (a, b), _ -> Printf.sprintf "E_Add (%s) (%s)" (exp_to_string a) (exp_to_string b)
  | ET.E_Var id, typ | ET.E_ImpVar id, typ -> Printf.sprintf "E_Var (%s): %s" id (typ_to_string typ)
  | ET.E_UnitVal, _ -> "T_Unit"
  | ET.E_Num i, _ -> Printf.sprintf "E_Num (%s)" (Int.to_string i)
  | ET.E_LetDyn { imp; init; body }, typ ->
      Printf.sprintf "E_LetDyn %s = (%s) in (%s): %s" imp (exp_to_string init) (exp_to_string body)
        (typ_to_string typ)

let rec type_check (p : Ast.prog) : ET.prog =
  let ctx = empty () in
  let decls, exp = p in
  let decls' = List.map (type_check_decl ctx) decls in
  let exp' = type_check_exp ctx exp in
  (decls', exp')

and type_check_exp (ctx : Context.t) (e : Ast.exp) : ET.exp =
  match e with
  | Ast.E_App (func, arg) -> (
      (* This is the APP rule from the notes *)
      (* Current context should be "r U t" *)
      (* Type checking the function makes the current context "r U t U s" *)
      let ((_, func_typ) as func') = type_check_exp ctx func in
      (* Type checking body gives makes the current context "r U t U s" 
         Even though the inference rules check body against "s" this is 
         fine because it is a subcoeffect of "r U t U s" *)
      let ((_, arg_typ) as arg') = type_check_exp ctx arg in
      match func_typ with
      | ET.T_Func { from; to_; imps } ->
          (* We don't add the latent params as they should already have been added
             by the parent of this node, we do ensure they are there though *)
          let () =
            assert_imps ctx imps;
            assert_subtype arg_typ from
          in
          (* The resulting context is "r U s U t" *)
          (E_App (func', arg'), to_)
      | _ -> raise (Failure (sprintf "Expected app of %s -> ..." (typ_to_string arg_typ))))
  | Ast.E_Add (e1, e2) ->
      let ((_, typ1) as e1') = type_check_exp ctx e1 in
      let ((_, typ2) as e2') = type_check_exp ctx e2 in
      let () = assert_subtype typ1 ET.T_Int in
      let () = assert_subtype typ2 ET.T_Int in
      (ET.E_Add (e1', e2'), ET.T_Int)
  | Ast.E_Var id -> (
      match get_var ctx id with
      | Some typ -> (ET.E_Var id, typ)
      | None -> raise (Failure (sprintf "Unbound variable '%s'" id)))
  | Ast.E_ImpVar id -> (
      (* This is the PARAM rule from the notes *)
      (* This rule will result in a context requiring a single flat coeffect *)
      match get_imp ctx id with
      | Some typ -> (ET.E_ImpVar id, typ)
      | None -> raise (Failure (sprintf "Unbound implicit parameter '%s'" id)))
  | Ast.E_UnitVal -> (ET.E_UnitVal, ET.T_Unit)
  | Ast.E_Num i -> (ET.E_Num i, ET.T_Int)
  | Ast.E_LetDyn { imp; init; body } ->
      (* This is the LETDYN rule from the notes *)
      (* Current context is "r" *)
      (* The initializer can modify the context, giving us "s' = r' \ { ?imp }" *)
      let ((_, init_typ) as init') = type_check_exp ctx init in
      (* We type check the body with "s = s' U { ?imp }" *)
      let () = add_imp ctx (imp, init_typ) in
      let ((_, typ) as body') = type_check_exp ctx body in
      let () = remove_imp ctx imp in
      (* The implicit param is removed from the set of required context
         The requirements in the context will be "s \ { ?imp }" *)
      (ET.E_LetDyn { imp; init = init'; body = body' }, typ)

and type_check_typ (ctx : Context.t) (t : Ast.typ) : ET.typ =
  match t with
  | Ast.T_Int -> ET.T_Int
  | Ast.T_UnitTyp -> ET.T_Unit
  | Ast.T_Func { from; to_; imps } ->
      let from' = type_check_typ ctx from in
      let to' = type_check_typ ctx to_ in
      let imps' = type_check_imps ctx imps in
      ET.T_Func { from = from'; to_ = to'; imps = imps' }

and type_check_imps (ctx : Context.t) (imps : (Ast.id * Ast.typ) list) =
  let type_check_typs (name, t) =
    let t' = type_check_typ ctx t in
    (name, t')
  in
  List.map type_check_typs imps

and type_check_param (ctx : Context.t) ((id, typ) : Ast.param) =
  let typ' = type_check_typ ctx typ in
  (id, typ')

and type_check_params_imps_and_ret_typ (ctx : Context.t) (params : Ast.param list)
    ?(imps : Ast.imp list = []) (ret_typ : Ast.typ) =
  let ret_typ' = type_check_typ ctx ret_typ in
  let imps' = type_check_imps ctx imps in
  let _aux param (typ, params', imps) =
    let ((_, param_typ) as param') = type_check_param ctx param in
    let typ' = ET.T_Func { from = param_typ; to_ = typ; imps } in
    (typ', param' :: params', [])
  in
  let typ', params', _ = List.fold_right _aux params (ret_typ', [], imps') in
  (params', imps', ret_typ', typ')

and type_check_decl (ctx : Context.t) (d : Ast.decl) : ET.decl =
  match d with
  | Ast.D_Val (id, exp) ->
      let ((_, typ) as exp') = type_check_exp ctx exp in
      let () = add_var ctx (id, typ) in
      ET.D_Val (id, exp')
  | Ast.D_Fun { name; params; imps; ret_typ; body } ->
      (* This is the ABS rule from the notes *)
      (* It is worth noting that we know at this point our implicit params
         cannot yet be defined lexically (they will always be dynamically bound). 
         This is quirk of the syntax surrounding function declarations. We will
         still treat this as a typical lambda abstraction *)
      let params', imps', ret_typ', typ =
        type_check_params_imps_and_ret_typ ctx params ~imps ret_typ
      in
      let () = add_vars ctx params' in
      let () = add_imps ctx imps' in
      let ((_, body_typ) as body') = type_check_exp ctx body in
      (* NOTE: Our body expression may have used any of these implicit parameters.
               But these will be latent requirements, we don't need them to create
               this function declaration, so remove them as well *)
      let () = remove_imps ctx (List.map fst imps') in
      let () = remove_vars ctx (List.map fst params) in
      let () = assert_subtype body_typ ret_typ' in
      let () = add_var ctx (name, typ) in

      ET.D_Fun { name; params = params'; imps = imps'; ret_typ = ret_typ'; body = body' }

and assert_imps (ctx : Context.t) (imps : ET.imp list) =
  if for_all (fun (i, _) -> is_some (Context.get_imp ctx i)) imps then ()
  else raise (Failure (sprintf "Missing context requirements: { %s }" (imps_to_string imps)))

and assert_subtype (typ1 : ET.typ) (typ2 : ET.typ) =
  (* TODO: Use subtyping rules instead of direct equality with types *)
  if typ1 = typ2 then ()
  else
    raise
      (Failure (sprintf "Expected type %s, but got %s" (typ_to_string typ2) (typ_to_string typ1)))
