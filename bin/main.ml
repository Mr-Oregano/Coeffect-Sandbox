open Coeffect_Sandbox
open Extchannel

let parse (in_ : in_channel) = Parser.parse (Lexer.lex (In_channel.to_seq in_))
let _ = In_channel.with_open_text "supplemental/samples/lex.lc" parse
