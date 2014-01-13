(* type location_t = Lexing.position * Lexing.position *)
(* type 'a p_elt =  *)
(*     { desc : 'a; *)
(*        loc : location_t  } *)

type prim_t =
  | Pbool of bool
  | Pint of int
  | Preal of float

type op_t =
  | Plus
  | Times

type p_method_t  = string option * p_term_t
and p_term_t =
  (* Pure untyped sigma calcul *)
  | PVariable of string
  | PObject of ( string * p_method_t) list
  | PSelection of p_term_t * string
  | PUpdate of p_term_t * string * p_method_t
  (* Extensions *)
  | PConst of prim_t
  | PMathExpr of op_t * p_term_t * p_term_t
  | PLambda of string * p_term_t
  | PApply of p_term_t * p_term_t
			    


