(* AST *)
module Ast = struct
  type id = string

  and exp =
    | E_App of (exp * exp)
    | E_Add of (exp * exp)
    | E_Var of id
    | E_ImpVar of id
    | E_UnitVal
    | E_Num of int
    | E_LetDyn of {
        imp : id;
        init : exp;
        body : exp;
      }

  and typ =
    | T_Int
    | T_UnitTyp
    | T_Func of {
        from : typ;
        to_ : typ;
        imps : (id * typ) list;
      }

  and param = {
    name : id;
    typ : typ;
    imps : (id * typ) list;
  }

  and decl =
    | D_Val of id * exp
    | D_Fun of {
        name : id;
        params : param list;
        ret_typ : typ;
        body : exp;
      }

  and prog = decl list * exp
end

(* Elaborated Tree *)
module ET = struct
  type id = string * typ
  and exp = Ast.exp * typ

  and typ =
    | ET_Int
    | ET_Unit
    | ET_Func of {
        from : typ;
        to_ : typ;
        imps : id list;
      }

  and param = {
    name : id;
    typ : typ;
    imps : (id * typ) list;
  }

  and decl =
    | ET_Val of id * exp
    | ET_Fun of {
        name : id;
        params : param list;
        ret_typ : id;
        body : exp;
      }

  type prog = decl list * exp
end

(* Interpreter Structures *)
module Interpreter = struct
  type value =
    | I_Num of int
    | I_Clo of (ET.id * ET.exp * env)

  and env = (ET.id * value) list
  and res = value option
end
