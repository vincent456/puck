open Parsed_terms
open Terms

module SMap = Utils.SMap

let arg_select x = Selection (Variable x, "arg")

let fun_object x body = 
  Object [("arg", (Some x,  arg_select x , [])); (*dummy init value *)
	  ("val", (Some x, body, []))]

let apply_obj b a =
  Selection ((Update ( b, "arg", (None, a, []))), "val")

(* TODO see if we need to take in account labels for substitution *)
let rec expanse env = function
  | PVariable v -> 
     begin
       try SMap.find v env 
       with Not_found -> Variable v 
     end
  | PObject o -> Object (List.map (fun (s, m) -> (s, expanse_method env m)) o)
       
  | PSelection (t,l)->  Selection (expanse env t, l)
  | PUpdate (t, l, m) -> Update (expanse env t, l, expanse_method env m)

  | PConst c -> Const c
  | PMathExpr (o, t1, t2) ->  MathExpr(o, expanse env t1, expanse env t2)
  | PLambda (b, t) -> 
     let env' = SMap.add b (arg_select b) env in
     fun_object b (expanse env' t)
			 
  | PApply (t1, t2) -> apply_obj (expanse env t1) (expanse env t2)

  and expanse_method env (b,t) = 
    let env' =  match b with 
      | None -> env
      | Some b' -> SMap.remove b' env
    in b, expanse env' t, []
				   
let expanse = expanse SMap.empty
