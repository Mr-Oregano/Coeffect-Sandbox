open Types
open Token

val prog_to_string : Ast.prog -> string
val decl_to_string : Ast.decl -> string
val typ_to_string : Ast.typ -> string
val exp_to_string : Ast.exp -> string
val parse : token Seq.t -> Ast.prog
