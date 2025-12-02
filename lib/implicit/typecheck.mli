open Types

val prog_to_string : ET.prog -> string
val decl_to_string : ET.decl -> string
val typ_to_string : ET.typ -> string
val exp_to_string : ET.exp -> string
val type_check : Ast.prog -> ET.prog
