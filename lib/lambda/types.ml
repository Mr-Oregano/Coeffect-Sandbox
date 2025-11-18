(* AST *)
type var = string

and exp =
  | E_Abs of (var * exp)
  | E_App of (exp * exp)
  | E_Var of var
  | E_Num of int

and prog = exp option

(* Interpreter Structures *)
type value =
  | I_Num of int
  | I_Clo of (var * exp * ctx)

and ctx = (var * value) list
and res = value option
