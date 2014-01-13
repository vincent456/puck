
open Terms
open Terms_printer

open Utils

exception Wrong of string * term_t

let replace_assoc key new_val l =
  let (l', replaced) = 
    List.fold_left begin 
	fun (acc, replaced) ((k, _) as p) -> 
	if key = k then
	  (k, new_val) :: acc, true
	else
	  p :: acc, replaced
      end ([], false) l
  in (List.rev l', replaced)	    

(* let rec substitute x obj t = *)
(*   let s = substitute x obj  *)
(*   and sm = substitute_method x obj in *)
(*   match t with *)
(*       | Const  _ -> t *)
(*       | Object  o -> Object (List.map (fun (l,def) -> l, sm def) o) *)
(*       | Variable v -> if v = x then obj *)
(* 		      else t *)
(*       | Selection (t, l) -> Selection (s t, l) *)
(*       | Update (t, l, def) -> Update (s t, l, sm def) *)
(*       | MathExpr (o,t1,t2) -> MathExpr(o, s t1, s t2) *)
(* and substitute_method x obj (b, t) = *)
(*   match b with *)
(*   | None -> (None, substitute x obj t) *)
(*   | Some b -> if b = x then (Some b, t) (\*si b = x, x masque l'autre définition*\) *)
(* 	      else (Some b, substitute x obj t) *)

let fresh_name =
  let id = ref 0 in
  (fun s -> 
   id := !id + 1;
   s ^ (string_of_int !id))

let interpret (defs : env_t) (t : term_t) : term_t=
  let lookup key env = 
    let rec f env = match env with
      | [] -> begin try [], List.assoc key defs
		    with Not_found -> raise (Wrong ("variable not found", Variable key))
	      end
      | (k, v) :: tl -> if key = k then tl, v
			else f tl
    in f env
  in
  let rec f env t = 
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "***                   Entering                                 *****\n"; *)
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "****************************************\n"; *)
    (* printf_term t; *)
    (* printf_env env; *)
    (* let t_init = t in *)
    (* let env, t =  *)
    match t with
      | Const  _
      | Object  _ -> env, t
      | Variable v -> lookup v env
      
      | Selection (t, label) -> 
	 begin match f env t with
	       | env, (Object o as obj) -> 
	 	  let (binder, body, env') = List.assoc label o 
		  in begin
		    match binder with
		    | None -> f  (env' @ env) body
		    | Some b -> 
		       f ( (b, obj) :: env' @ env) body
		    end
	       | _, w -> raise (Wrong ("object expected", w))
	 end
	   
      | Update (t, label, (bind, body, env')) -> 
	 begin
	   match f env t with
	   | env_out, Object original_obj -> 
	      begin
		let (new_obj, replaced) = replace_assoc label (bind, body, env @ env') original_obj
		in if replaced then
		     env_out, Object new_obj
		   else
		     raise (Wrong ("label " ^ label ^ "expected ", Object original_obj ))
	      end
	   | _, w -> raise (Wrong ("object expected", w))
	 end
      | MathExpr (o,t1,t2) -> i_math env (o,t1,t2)
    (* in *)
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "***                   Leaving                                 *****\n"; *)
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "****************************************\n"; *)
    (* Format.printf "entered with :"; *)
    (* printf_term t_init; *)
    (* Format.printf "leaving with :"; *)
    (* printf_term t; *)
    (* printf_env env; *)
    (* env,t *)
  and i_math env (op, t1, t2) = 
    let _, t1 = f env t1 in
    let env, t2 = f env t2 in
    match (t1, t2) with
	 | Const c1, Const c2 -> 
	    env, Parsed_terms.(begin match (op, c1, c2) with
				| Plus, Pint i1, Pint i2 -> Const (Pint (i1 + i2))
				| Plus, Preal r1, Preal r2 -> Const (Preal ( r1 +. r2))
				| Times, Pint i1, Pint i2 -> Const (Pint (i1 / i2))
				| Times, Preal r1, Preal r2 -> Const (Preal ( r1 /. r2))
				| _ -> raise (Wrong ("Type error", MathExpr (op, Const c1, Const c2) ))
			  end)
	 | (t1', t2') -> raise (Wrong ("expect const in math expr", MathExpr (op, t1', t2') ))
  in
  let _, t =f [] t in (* throwing out the resulting env*)
  t
