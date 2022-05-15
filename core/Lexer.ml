open Sedlexing

type token = Parser.token

open Parser

exception LexError of Lexing.position * string

let blank = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let any_blank = [%sedlex.regexp? blank | newline]
let digit = [%sedlex.regexp? '0' .. '9']
let unsigned_integer = [%sedlex.regexp? digit, Star digit]
let lower_case_letter = [%sedlex.regexp? 'a' .. 'z']
let upper_case_letter = [%sedlex.regexp? 'A' .. 'Z']
let letter = [%sedlex.regexp? lower_case_letter | upper_case_letter]
let alphanum = [%sedlex.regexp? lower_case_letter | digit | '_']
let regular_ident = [%sedlex.regexp? lower_case_letter, Star alphanum]
let modal_ident = [%sedlex.regexp? lower_case_letter, Star alphanum, '\'']
let type_ident = [%sedlex.regexp? upper_case_letter, Star alphanum | '_']
let rec nom buf = match%sedlex buf with Plus any_blank -> nom buf | _ -> ()

let token buf =
  nom buf;
  match%sedlex buf with
  | eof -> EOF
  | "" -> EOF
  | "[]" | 0x25A1 -> TBOX
  | "->" | 0x2192 -> ARROW
  | "=>" | 0x21D2 | '.' -> DARROW
  | "=" -> EQ
  | "()" -> UNIT
  | "Nat" | 0x2115 -> TNAT
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '<' -> LANGLE
  | '>' -> RANGLE
  | ':' -> COLON
  | ',' -> COMMA
  | '*' | 0x00D7 -> CROSS
  | '+' -> PLUS
  | '-' -> MINUS
  | '/' -> SLASH
  | '|' -> PIPE
  | "fst" | 0x03C0, 0x2081 (* π₁ *) -> FST
  | "snd" | 0x03C0, 0x2082 (* π₂ *) -> SND
  | "in" -> IN
  | "fun" | 0x03BB -> FUN
  | "let" -> LET
  | "box" -> BOX
  | "letbox" -> LETBOX
  | "match" -> MATCH
  | "with" -> WITH
  | "end" -> END
  | "zero" -> ZERO
  | "succ" -> SUCC
  | unsigned_integer -> UINTZ (Nat.of_string (Utf8.lexeme buf))
  | regular_ident -> IDR (Utf8.lexeme buf)
  | modal_ident -> IDM (Utf8.lexeme buf)
  | type_ident -> IDT (Utf8.lexeme buf)
  | _ ->
      let position = fst @@ lexing_positions buf in
      let tok = Utf8.lexeme buf in
      raise @@ LexError (position, Printf.sprintf "unexpected character %S" tok)

let lexer buf = Sedlexing.with_tokenizer token buf
