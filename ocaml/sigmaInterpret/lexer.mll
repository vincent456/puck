{

  open Lexing
  open Parser

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [
	"S", SIGMA;
	(* "ς", SIGMA; *)
	"L", LAMBDA;
	(* "λ", LAMBDA; *)
	"and", AND;
	"false", CONST_BOOL(false);
	"let", LET;
	"in", IN;
	"true", CONST_BOOL(true);
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
	  | digit+ exponent
let ident = (alpha | '_' ) (alpha | '_' | digit)*

rule main = parse
  | '\n'
      { newline lexbuf; main lexbuf }
  | [' ' '\t' '\r']+
      { main lexbuf }
  | "--" [^ '\n']* ['\n']
      { newline lexbuf; main lexbuf }
  | "/*"
      { comment lexbuf; main lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | float
      { CONST_REAL (float_of_string (lexeme lexbuf)) }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "."
      { DOT }
  | "="
      { EQUAL}
  | ":"
      {COLON}
  | ","
      { COMMA }
  | "<-"
      { LARROW }
  | _
      { raise (Lexical_error (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
