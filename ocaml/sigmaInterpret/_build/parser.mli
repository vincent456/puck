exception Error

type token = 
  | STAR
  | SIGMA
  | RPAREN
  | RBRACKET
  | PLUS
  | LPAREN
  | LET
  | LBRACKET
  | LARROW
  | LAMBDA
  | IN
  | IDENT of (string)
  | EQUAL
  | EOF
  | DOT
  | CONST_REAL of (float)
  | CONST_INT of (int)
  | CONST_BOOL of (bool)
  | COMMA
  | COLON
  | AND


val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ((string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t)