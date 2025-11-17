open Char.Ascii
open Seq
open Token

let token_to_string (t : token) =
  match t with
  | LParen -> "("
  | RParen -> ")"
  | Slash -> "\\"
  | Period -> "."
  | Colon -> ":"
  | Semicolon -> ";"
  | Arrow -> "->"
  | Equals -> "="
  | Plus -> "+"
  | KW_Fun -> "fun"
  | KW_Val -> "val"
  | KW_Int -> "int"
  | KW_Unit -> "unit"
  | KW_Letdyn -> "letdyn"
  | KW_In -> "in"
  | Implicit x -> Printf.sprintf "ImpVar (%s)" x
  | Variable x -> Printf.sprintf "Var (%s)" x
  | Literal n -> Printf.sprintf "Lit (%d)" n

(* TODO: Rather than immediately raising errors, queue them into a list *)
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
  | Some (':', cs') -> Some (Colon, cs')
  | Some (';', cs') -> Some (Semicolon, cs')
  | Some ('=', cs') -> Some (Equals, cs')
  | Some ('+', cs') -> Some (Plus, cs')
  | Some ('?', cs') -> Some (lex_imp_id cs')
  | Some ('-', cs') -> (
      match uncons cs' with
      | Some ('>', cs'') -> Some (Arrow, cs'')
      | _ -> raise (Failure "Unexpected character"))
  | Some (c, cs') ->
      if is_digit c then
        (* Consume all digit characters *)
        Some (lex_literal c cs')
      else if is_letter c then
        (* Consume all alpha or digit characters *)
        Some (lex_id_or_keyword c cs')
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

and lex_literal (c : char) (cs : char Seq.t) =
  let value, seq = lex_value is_digit c cs in
  (Literal (int_of_string value), seq)

and lex_id (c : char) (cs : char Seq.t) =
  let value, seq = lex_value is_alphanum c cs in
  (Variable value, seq)

and lex_imp_id (cs : char Seq.t) =
  let value, seq = lex_value is_alphanum '?' cs in
  (Implicit value, seq)

and lex_id_or_keyword (c : char) (cs : char Seq.t) =
  let value, seq = lex_id c cs in
  match value with
  | Variable "fun" -> (KW_Fun, seq)
  | Variable "val" -> (KW_Val, seq)
  | Variable "int" -> (KW_Int, seq)
  | Variable "unit" -> (KW_Unit, seq)
  | Variable "letdyn" -> (KW_Letdyn, seq)
  | Variable "in" -> (KW_In, seq)
  | _ -> (value, seq)
