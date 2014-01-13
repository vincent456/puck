type prim_t = Parsed_terms.prim_t

type op_t = Parsed_terms.op_t

type env_t = (string * term_t) list
and method_t  = string option * term_t * env_t
and term_t = 
  (* Pure untyped sigma calcul *)
  | Variable of string
  | Object of ( string * method_t) list
  | Selection of term_t * string
  | Update of term_t * string * method_t

   (* Extensions *)
  | Const of prim_t
  | MathExpr of op_t * term_t * term_t	       

