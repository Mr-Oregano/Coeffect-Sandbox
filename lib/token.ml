type token =
  | T_LParen
  | T_RParen
  | T_Slash
  | T_Period
  | T_Colon
  | T_Semicolon
  | T_Arrow
  | T_Equals
  | T_Plus
  | T_Exclamation
  | T_Fun
  | T_Val
  | T_IntTyp
  | T_UnitTyp
  | T_LetDyn
  | T_In
  | T_ImpVar of string
  | T_Var of string
  | T_Num of int
  | T_UnitVal
