open Char.Ascii
open Seq
open Token

let token_to_string (t : token) =
  match t with
  | TK_LParen -> "("
  | TK_RParen -> ")"
  | TK_LCurly -> "{"
  | TK_RCurly -> "}"
  | TK_Slash -> "\\"
  | TK_Period -> "."
  | TK_Comma -> ","
  | TK_Colon -> ":"
  | TK_Semicolon -> ";"
  | TK_Arrow -> "->"
  | TK_Equals -> "="
  | TK_Plus -> "+"
  | TK_Exclamation -> "!"
  | TK_KW_Fun -> "fun"
  | TK_KW_Val -> "val"
  | TK_KW_Int -> "int"
  | TK_KW_Unit -> "unit"
  | TK_KW_LetDyn -> "letdyn"
  | TK_KW_In -> "in"
  | TK_ImpVar x -> Printf.sprintf "ImpVar (%s)" x
  | TK_Var x -> Printf.sprintf "Var (%s)" x
  | TK_Num n -> Printf.sprintf "Lit (%d)" n
  | TK_Unit -> "()"

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
  | Some ('(', cs') -> (
      match uncons cs' with
      | Some (')', cs'') -> Some (TK_Unit, cs'')
      | Some (c, cs'') -> Some (TK_LParen, append (singleton c) cs'')
      | None -> Some (TK_LParen, empty))
  | Some (')', cs') -> Some (TK_RParen, cs')
  | Some ('{', cs') -> Some (TK_LCurly, cs')
  | Some ('}', cs') -> Some (TK_RCurly, cs')
  | Some ('\\', cs') -> Some (TK_Slash, cs')
  | Some ('.', cs') -> Some (TK_Period, cs')
  | Some (',', cs') -> Some (TK_Comma, cs')
  | Some (':', cs') -> Some (TK_Colon, cs')
  | Some (';', cs') -> Some (TK_Semicolon, cs')
  | Some ('=', cs') -> Some (TK_Equals, cs')
  | Some ('+', cs') -> Some (TK_Plus, cs')
  | Some ('#', cs') ->
      (* This is a comment, no tokens to generate, skip to the next line *)
      let is_not_newline = fun c -> c <> '\n' in
      let cs'' = Seq.drop_while is_not_newline cs' in
      lex_next cs''
  | Some ('!', cs') -> Some (TK_Exclamation, cs')
  | Some ('?', cs') -> Some (lex_imp_id cs')
  | Some ('-', cs') -> (
      match uncons cs' with
      | Some ('>', cs'') -> Some (TK_Arrow, cs'')
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
  (TK_Num (int_of_string value), seq)

and lex_id (c : char) (cs : char Seq.t) =
  let value, seq = lex_value is_alphanum c cs in
  (TK_Var value, seq)

and lex_imp_id (cs : char Seq.t) =
  let value, seq = lex_value is_alphanum '?' cs in
  (TK_ImpVar value, seq)

and lex_id_or_keyword (c : char) (cs : char Seq.t) =
  let value, seq = lex_id c cs in
  match value with
  | TK_Var "fun" -> (TK_KW_Fun, seq)
  | TK_Var "val" -> (TK_KW_Val, seq)
  | TK_Var "int" -> (TK_KW_Int, seq)
  | TK_Var "unit" -> (TK_KW_Unit, seq)
  | TK_Var "letdyn" -> (TK_KW_LetDyn, seq)
  | TK_Var "in" -> (TK_KW_In, seq)
  | _ -> (value, seq)
