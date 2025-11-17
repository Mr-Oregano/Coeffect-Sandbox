type token =
  | LParen
  | RParen
  | Slash
  | Period
  | Colon
  | Semicolon
  | Arrow
  | Equals
  | Plus
  | KW_Fun
  | KW_Val
  | KW_Int
  | KW_Unit
  | KW_Letdyn
  | KW_In
  | Implicit of string
  | Variable of string
  | Literal of int
