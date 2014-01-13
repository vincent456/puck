{

  open Lexing
  open Parser

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 7 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [
	"contains", CONTAINS;
	"edge", EDGE;
	"hideFrom", HIDEFROM;
	"isa", ISA;
	"isFriendOf", ISFRIENDOF;
	"node", NODE;
	"uses", USES
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

let ident = ( alpha | '_' ) (alpha | '_' | '.' | '#' | digit)*

let quoted = '\'' ([^'\''])* '\''

rule main = parse
  | '\n'
      { newline lexbuf; main lexbuf }
  | [' ' '\t' '\r']+
      { main lexbuf }
  | "%" [^ '\n']* ['\n']
      { newline lexbuf; main lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | quoted
      { let s = (lexeme lexbuf) in
	QUOTED_IDENT ( String.sub  s 1 ((String.length s) -2) ) 
	(* remove the quotes *) }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "."
      { DOT }
  | ","
      { COMMA }
  | _
      { raise (Lexical_error (lexeme lexbuf)) }
  | eof
      { EOF }

and quoted = parse
  | '\''  { main lexbuf }
  | '\n' { newline lexbuf; quoted lexbuf }
  | [^'\'']* { IDENT (lexeme lexbuf) }
  | eof  { raise (Lexical_error "unterminated quote") }
