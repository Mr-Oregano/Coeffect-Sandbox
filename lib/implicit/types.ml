(* AST *)
module Ast = struct
  type id = string

  and exp =
    | E_Abs of {
        param : param;
        imps : imp list;
        body : exp;
      }
    | E_App of (exp * exp)
    | E_Add of (exp * exp)
    | E_Var of id
    | E_ImpVar of id
    | E_Unit
    | E_Num of int
    | E_LetDyn of {
        imp : id;
        init : exp;
        body : exp;
      }

  and typ =
    | T_Int
    | T_Unit
    | T_Func of {
        from : typ;
        to_ : typ;
        imps : (id * typ) list;
      }

  and param = id * typ
  and imp = id * typ

  and decl = {
    name : id;
    exp : exp;
    typ_opt : typ option;
  }

  and prog = decl list * exp
end

(* Elaborated Tree *)
module ET = struct
  type id = string
  and exp = exp_t * typ

  and exp_t =
    | E_Abs of {
        param : param;
        imps : imp list;
        body : exp;
      }
    | E_App of (exp * exp)
    | E_Add of (exp * exp)
    | E_Var of id
    | E_ImpVar of id
    | E_Unit
    | E_Num of int
    | E_LetDyn of {
        imp : id;
        init : exp;
        body : exp;
      }

  and typ =
    | T_Int
    | T_Unit
    | T_Func of {
        from : typ;
        to_ : typ;
        imps : (id * typ) list;
      }

  and param = id * typ
  and imp = id * typ

  and decl = {
    name : id;
    exp : exp;
  }

  type prog = decl list * exp
end

(* Interpreter Structures *)
module Interp = struct
  type id = string
  and binding = id * value

  and value =
    | V_Unit
    | V_Num of int
    | V_Clo of clo

  and clo = {
    param_id : ET.id;
    body : ET.exp;
    env : binding list;
    imps : binding list;
  }

  and res = value
end
