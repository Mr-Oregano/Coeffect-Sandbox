open Char.Ascii
open Seq

type token =
  | LParen
  | RParen
  | Slash
  | Period
  | Variable of string
  | Literal of int

let token_to_string (t : token) =
  match t with
  | LParen -> "("
  | RParen -> ")"
  | Slash -> "\\"
  | Period -> "."
  | Variable x -> Printf.sprintf "Var (%s)" x
  | Literal n -> Printf.sprintf "Lit (%d)" n

(* TODO: Rather than immediately raising this, queue this into a list of errors *)
let rec lex (cs : char Seq.t) =
  once
    (unfold
       (fun cs ->
         let next = lex_next cs in
         match next with None -> None | Some (tok, cs') -> Some (tok, cs'))
       cs)

and lex_next (cs : char Seq.t) : (token * char Seq.t) option =
  match uncons cs with
  | None -> None
  | Some ('(', cs') -> Some (LParen, cs')
  | Some (')', cs') -> Some (RParen, cs')
  | Some ('\\', cs') -> Some (Slash, cs')
  | Some ('.', cs') -> Some (Period, cs')
  | Some (c, cs') ->
      if is_digit c then
        (* Consume all digit characters *)
        Some (lex_digit c cs')
      else if is_letter c then
        (* Consume all alpha or digit characters *)
        Some (lex_alpha c cs')
      else if is_white c then
        (* Skip this white space, continue to the next *)
        lex_next cs'
      else raise (Failure "Unexpected character")

and lex_value (pred : char -> bool) (c : char) (cs : char Seq.t) =
  (* We need to use a mutable buffer to construct a string out of the 
     sequence of characters we are about to consume *)
  let buf = Buffer.create 10 in

  (* We need a specialized function here that consumes characters from
     the sequence so long as they satisfy the predicate, adds them to the
     buffer, and returns the remainder of the sequence when done *)
  let rec _aux_pred (cs' : char Seq.t) =
    match uncons cs' with
    | None -> empty
    | Some (c, cs'') ->
        if pred c then (
          Buffer.add_char buf c;
          _aux_pred cs'')
        (* To simulate a look-ahead of 1 with a stream that is forward-only
           then we have to prepend the character we just read to the front *)
          else append (singleton c) cs''
  in

  let () = Buffer.add_char buf c in
  let cs' = _aux_pred cs in
  (Buffer.contents buf, cs')

and lex_digit (c : char) (cs : char Seq.t) =
  let value, seq = lex_value is_digit c cs in
  (Literal (int_of_string value), seq)

and lex_alpha (c : char) (in_ : char Seq.t) =
  let value, seq = lex_value is_alphanum c in_ in
  (Variable value, seq)
