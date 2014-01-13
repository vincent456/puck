%{
  open List_access_graph
  let nodes = ref []
  let edges = ref []
  let constraints = ref []
			
  let add_node n = nodes := n :: !nodes
  let add_node3 (uid,_,nick) = add_node (uid,nick)
  let add_node4 (uid,_,nick,_) = add_node (uid,nick)

  let add_uses (n1,n2) = edges := (Uses (n1,n2)) :: !edges
  let add_contains (n1,n2) = edges := (Contains (n1,n2)) :: !edges

  let add_hiddenFrom (n1,n2) = constraints := (HideFrom (n1,n2)) :: !constraints
  let add_isFriendOf (n1,n2) = constraints := (IsFriendOf (n1,n2)) :: !constraints
							     
%}

%token COMMA
%token CONTAINS
%token DOT
%token EDGE
%token EOF
%token HIDEFROM
%token <string> IDENT
%token ISA
%token ISFRIENDOF
%token LPAREN
%token NODE
%token <string> QUOTED_IDENT
%token RPAREN
%token USES

%start file
%type <((string * string) list) * (List_access_graph.edge_t list) * (List_access_graph.constraint_t list)> file

%%

file: graph EOF { !nodes, !edges, !constraints }
;

elt : 
| s = IDENT { s }
| s = QUOTED_IDENT { s }
;

parser_pair : 
LPAREN  value1 = elt  COMMA  value2 = elt RPAREN
					  { value1, value2 }
;

triplet:
LPAREN  value1 = elt  COMMA  value2 = elt  COMMA value3 = elt  RPAREN
					 { value1, value2, value3 }
;

quadruplet :
LPAREN  value1 = elt  COMMA  value2 = elt  COMMA value3 = elt COMMA value4 = elt RPAREN
					 { value1, value2, value3, value4 }
;


graph :
| (* empty *) { () }
| NODE  p = parser_pair DOT graph  { add_node p }
| NODE t = triplet DOT graph { add_node3 t } 
| NODE q = quadruplet DOT graph {add_node4 q}

| USES  p = parser_pair DOT graph   { add_uses p }
| CONTAINS  p = parser_pair DOT graph {add_contains p }

| EDGE LPAREN USES COMMA value1 = elt COMMA value2 = elt RPAREN DOT graph {add_uses (value1,value2) }
| EDGE LPAREN ISA COMMA value1 = elt COMMA value2 = elt RPAREN DOT graph {add_uses (value1,value2) }
| EDGE LPAREN CONTAINS COMMA value1 = elt COMMA value2 = elt RPAREN DOT graph {add_contains (value1,value2) }


| HIDEFROM p = parser_pair DOT graph  {add_hiddenFrom p }
| ISFRIENDOF p = parser_pair DOT  graph {add_isFriendOf p }
;


