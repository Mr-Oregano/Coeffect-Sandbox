(* AST *)
type var = string

and exp =
  | Abs of (var * exp)
  | App of (exp * exp)
  | Var of var
  | Num of int

and prog = exp option

(* Interpreter Structures *)
type value =
  | Num of int
  | Clo of (var * exp * ctx)

and ctx = (var * value) list
and res = value option
