type token =
  | TK_LParen
  | TK_RParen
  | TK_LCurly
  | TK_RCurly
  | TK_Slash
  | TK_Period
  | TK_Comma
  | TK_Colon
  | TK_Semicolon
  | TK_Arrow
  | TK_Equals
  | TK_Plus
  | TK_Exclamation
  | TK_KW_Fun
  | TK_KW_Val
  | TK_KW_Int
  | TK_KW_Unit
  | TK_KW_LetDyn
  | TK_KW_In
  | TK_ImpVar of string
  | TK_Var of string
  | TK_Num of int
  | TK_Unit
