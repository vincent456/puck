
open Parsed_terms

type prim_t = Parsed_terms.prim_t
type op_t = Parsed_terms.op_t

type 'a binded =  
    { mutable binder : 'a;
      mutable body: mp_term_t }
 and mp_method_t = (string option) binded 
 and mp_var_t = {mutable v: string }
 and mp_obj_t = { mutable methods : (string * mp_method_t) list }
 and mp_sel_t = 
   { mutable receiver : mp_term_t;
      mutable label : string }
 and mp_upd_t =
   { sel : mp_sel_t; 
     mutable new_val : mp_method_t }
 and mp_lamb_t = string binded
 and mp_term_t = 
   (* Pure untyped sigma calcul *)
   | MPVariable of mp_var_t
   | MPObject of mp_obj_t
   | MPSelection of mp_sel_t
   | MPUpdate of mp_upd_t 
   (* Extensions *)
   | MPConst of prim_t
   | MPMathExpr of op_t * mp_term_t * mp_term_t	       
   | MPLambda of mp_lamb_t
   | MPApply of mp_term_t * mp_term_t


let rec mp_term_of_p_term = function
  | PVariable s -> MPVariable {v = s}
  | PObject ms -> MPObject {methods = List.map (fun (n, (bi,bo))  -> (n, {binder = bi; body = mp_term_of_p_term bo})) ms}
  | PSelection (pt,l) -> MPSelection {receiver =  mp_term_of_p_term pt; label = l}
  | PUpdate (pt1,l,(b, pt2)) -> MPUpdate {sel = { receiver = mp_term_of_p_term pt1; label =l};
					new_val = {binder =b; body = mp_term_of_p_term pt2}}
  (* Extensions *)
  | PConst c -> MPConst c
  | PMathExpr (o, pt1, pt2) -> MPMathExpr (o, mp_term_of_p_term pt1, mp_term_of_p_term pt2)
  | PLambda (b, pt) -> MPLambda { binder = b; body = mp_term_of_p_term pt}
  | PApply (pt1, pt2) -> MPApply (mp_term_of_p_term pt1, mp_term_of_p_term pt2)

  
