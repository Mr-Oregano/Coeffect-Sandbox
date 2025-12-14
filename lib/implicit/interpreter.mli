open Types.Interp
open Types

val res_to_string : res -> string
val value_to_string : value -> string
val eval : ET.prog -> res
