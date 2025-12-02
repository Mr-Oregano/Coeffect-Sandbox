open Types.ET
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

  let add_var ctx (id, typ) =
    assert_not_contains ctx.vars id;
    replace ctx.vars id typ

  let add_vars ctx ids = List.iter (add_var ctx) ids

  let add_imp ctx (id, typ) =
    assert_not_contains ctx.imps id;
    replace ctx.imps id typ

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
