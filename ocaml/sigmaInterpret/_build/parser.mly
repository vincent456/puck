%{
  open Parsed_terms
  open Parsed_terms_printer
  (* let loc () = symbol_start_pos (), symbol_end_pos () *)
  (* let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () } *)

%}

%token COLON
%token COMMA
%token <bool> CONST_BOOL
%token <int> CONST_INT
%token <float> CONST_REAL
%token AND
%token DOT
%token EQUAL
%token <string> IDENT
%token IN
%token LAMBDA
%token LARROW
%token LBRACKET
%token LET
%token LPAREN
%token PLUS
%token RBRACKET
%token RPAREN
%token SIGMA
%token STAR
%token EOF

(*lowest precedence*)
%right binder
%left assign
%left PLUS
%left STAR
%left DOT 

(* higher precedence *)

(* Point d'entrée *)

%start file
%type <(string * Parsed_terms.p_term_t) list * Parsed_terms.p_term_t> file

%%

file: p = program EOF { p }
;

program:
| (* empty *)  { ([], PObject []) }
| d = defs t = term    { (d, t) }
;

defs:
| (* empty *) { [] }
| LET ds = def_list IN { ds }
;

def_list:
| d = def  { [d] }
| d = def AND ds = def_list { d :: ds } 
;

def:
 label = IDENT EQUAL t = term {  label, t }
;

const:
| c = CONST_BOOL { Pbool c }
| c = CONST_INT {  Pint c }
| c = CONST_REAL { Preal c }
;

term0:
| LPAREN t = term RPAREN 
	{ t }
| i = IDENT 
	{ PVariable i }
| c = const 
	{ PConst c }
| LBRACKET ms = method_list RBRACKET 
	{ PObject ms }
;

term1:
| t = term0 
    { t }

| t = term DOT label = IDENT 
	{ PSelection (t, label) }

| t1 = term1   t2 = term0 
	{ PApply (t1, t2) }
;

term:
| t = term1
	{ t }

| t = term DOT label = IDENT m = update 
        { PUpdate(t, label, m) }

| LAMBDA LPAREN b = IDENT RPAREN t = term %prec binder
	{ PLambda (b, t)}
| e = mathExpr 
	{ e }
;

mathExpr:
| t1 = term PLUS t2 = term { PMathExpr (Plus, t1, t2) }
| t1 = term STAR t2 = term { PMathExpr (Times, t1, t2) }
;

update :
| LARROW m = method_def 
	       { m }
| COLON EQUAL t = term   %prec assign 
	       { None, t }
;

method_list:
| (* empty *) { [] }
| m = method_decl 
	{ [m] }
| m = method_decl COMMA ms = method_list {  m :: ms }
;

method_decl:
| label  = IDENT EQUAL m = method_def 
	    { label, m }
| label  = IDENT EQUAL m = term 
            { label, (None, m)}
;

method_def:
| SIGMA LPAREN b = IDENT RPAREN t = term %prec binder
	{  Some b, t }
;

