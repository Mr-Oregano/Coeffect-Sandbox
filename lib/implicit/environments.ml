open Types.ET
open Types.Interp
open Hashtbl

module Context = struct
  (* The context for type checking can be emulated with a pair of hash tables
     We have the context of free variables annotated with the flat coeffect context *)
  (* TODO: Is a mutable context really a good idea? *)
  type t = {
    vars : (id, typ) Hashtbl.t;
    imps : (id, typ) Hashtbl.t;
  }

  let empty () = { vars = create 5; imps = create 5 }

  (* TODO: Allow identifier shadowing *)
  let assert_not_contains ht id =
    if find_opt ht id = None then ()
    else raise (Failure (Printf.sprintf "Already bound identifier '%s'" id))

  (* TODO: Consider what it means to bind two or more implicit parameters with the same name
           but different types. Do we use subtyping?*)
  let assert_not_contains_or_same_typ ht id typ =
    match find_opt ht id with
    | None -> ()
    | Some typ' ->
        if typ = typ' then ()
        else raise (Failure (Printf.sprintf "Already bound identifier '%s'" id))

  let add_var ctx (id, typ) =
    assert_not_contains ctx.vars id;
    replace ctx.vars id typ

  let add_vars ctx ids = List.iter (add_var ctx) ids

  let add_imp ctx (id, typ) =
    assert_not_contains_or_same_typ ctx.imps id typ;
    add ctx.imps id typ

  let add_imps ctx ids = List.iter (add_imp ctx) ids
  let remove_var ctx name = remove ctx.vars name
  let remove_vars ctx names = List.iter (remove_var ctx) names
  let remove_imp ctx name = remove ctx.imps name
  let remove_imps ctx names = List.iter (remove_imp ctx) names
  let get_var ctx name = find_opt ctx.vars name
  let get_vars ctx = to_seq ctx.vars
  let get_imp ctx name = find_opt ctx.imps name
  let get_imps ctx = to_seq ctx.imps
end

module Env = struct
  type t = {
    vars : (id, value) Hashtbl.t;
    imps : (id, value) Hashtbl.t;
  }

  (* TODO: Allow identifier shadowing. *)
  let assert_not_contains ht id =
    if find_opt ht id = None then ()
    else raise (Failure (Printf.sprintf "Already bound identifier '%s'" id))

  (* Important note: the to_seq function of Hashtbl returns the duplicates/shadowed 
     keys in REVERSE order. This is important to consider when using this to get 
     lexical environments! Probably not ideal to handle it this way, but this is
     just for a tree-walk interpreter anyways *)
  let to_seq_latest tbl =
    let seen = Hashtbl.create (Hashtbl.length tbl) in
    let _aux (k, v) = if Hashtbl.mem seen k then () else Hashtbl.replace seen k v in
    let () = Seq.iter _aux (Hashtbl.to_seq tbl) in
    to_seq seen

  let empty () = { vars = create 5; imps = create 5 }

  let add_var env (id, v) =
    assert_not_contains env.vars id;
    replace env.vars id v

  let add_vars env ids = List.iter (add_var env) ids
  let add_var_shadow env (id, v) = add env.vars id v
  let add_vars_shadow env ids = List.iter (add_var_shadow env) ids

  let add_imp env (id, v) =
    assert_not_contains env.imps id;
    replace env.imps id v

  let add_imps env ids = List.iter (add_imp env) ids
  let add_imp_shadow env (id, v) = add env.imps id v
  let add_imps_shadow env ids = List.iter (add_imp_shadow env) ids
  let get_var env id = find_opt env.vars id
  let get_vars env = List.of_seq (to_seq_latest env.vars)
  let get_imp env id = find_opt env.imps id
  let get_imps env = List.of_seq (to_seq_latest env.imps)
  let remove_var env id = remove env.vars id
  let remove_vars env names = List.iter (remove_var env) names
  let remove_imp env id = remove env.imps id
  let remove_imps env names = List.iter (remove_imp env) names

  let get_imps_excluded env names_excluded =
    let _aux names (id, _) = Option.is_none (Seq.find (( = ) id) names) in
    let s = Seq.filter (_aux names_excluded) (to_seq_latest env.imps) in
    List.of_seq s
end
