(* AST *)
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
  | T_ImpTyp of typ
  | T_Func of typ * typ

and decl =
  | D_Val of id * exp
  | D_Fun of {
      name : id;
      params : (id * typ) list;
      ret_typ : typ;
      body : exp;
    }

and prog = decl list * exp

(* Interpreter Structures *)
type value =
  | I_Num of int
  | I_Clo of (id * exp * ctx)

and ctx = (id * value) list
and res = value option
