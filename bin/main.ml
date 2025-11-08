open Coeffect_Sandbox
open Extchannel

let pp_token (tok : Lexer.token) =
  match tok with
  | LParen -> print_endline "LParen"
  | RParen -> print_endline "RParen"
  | Slash -> print_endline "Slash"
  | Period -> print_endline "Period"
  | Variable s -> print_endline ("Var (" ^ s ^ ")")
  | Literal i -> print_endline ("Literal (" ^ Int.to_string i ^ ")")

let parse (in_ : in_channel) =
  Seq.iter pp_token (Lexer.lex (In_channel.to_seq in_))

let () = In_channel.with_open_text "supplemental/samples/lex.lc" parse
