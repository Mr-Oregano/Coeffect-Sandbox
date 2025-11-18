open Types
open Token

val prog_to_string : prog -> string
val decl_to_string : decl -> string
val typ_to_string : typ -> string
val exp_to_string : exp -> string
val parse : token Seq.t -> prog
