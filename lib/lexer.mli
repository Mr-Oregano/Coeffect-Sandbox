open Token

val token_to_string : token -> string
val lex : char Seq.t -> token Seq.t
