open List_access_graph
open Parsed_terms

module SMap = Utils.SMap

let scope_str = "."

let add_method_def add_term container get_object_container env (nodes, arcs) (binder, body) =
  let object_container = get_object_container body in
  let nodes, arcs, env = match binder with
    | None -> nodes, arcs, env
    | Some b -> 
       let full_b = container ^ scope_str ^ b in
       let b_node = (full_b, b) in
       b_node :: nodes, Contains (container, full_b ) :: arcs, SMap.add b object_container  env
  in
  
  add_term  container object_container env (nodes,  arcs) body


let add_method_to_graph add_term container object_container env (nodes, arcs) (label, m) =
  let full_label = container ^ scope_str ^ label in
  let label_node = full_label, label in
  let nodes, arcs, env = 
    label_node :: nodes, 
    Contains (container, full_label) :: arcs,
    SMap.add label full_label env (*full_label or object_container ... is it even necessary to add something to env ?? *)
  in

  let get_object_container body = match body with
    | PObject _ -> full_label
    | _ -> object_container
  in
  
  add_method_def add_term full_label get_object_container env (nodes, arcs) m

		 

(* let rec add_term_to_graph container ((nodes, arcs) as graph) t = *)
(*   let add_term = add_term_to_graph container in *)
(*   match t with *)
(*   | Variable _ -> graph *)
(*   | Object method_list -> *)
(*      List.fold_left (add_method_to_graph add_term_to_graph container) graph method_list *)
(*   | Selection (t, l) ->  add_term (nodes,  Uses (container, l) :: arcs) t *)
			
(*   | Update (t, l, (_, body)) -> (\* correct ? *\) *)
(*      let g = add_term (nodes,  Uses (container, l) :: arcs)  body *)
(*      in add_term g t *)
			   
(*   | Const _ -> graph *)
(*   | MathExpr (_, t1, t2) -> add_term (add_term graph t1) t2 *)

(* let ag_of_program (env, t) =  *)
(*   let r = "root" in *)
(*   let init_graph = [r], [] in *)
(*   let g = List.fold_left  *)
(* 	    begin fun (nodes, arcs) (n,t) ->  *)
(* 		  add_term_to_graph n (nodes, Contains(r, n) :: arcs) t  *)
		  
(* 	    end init_graph env in *)
(*   add_term_to_graph r g t *)


(* HYPOTHESE : tous les identifiants sont unique !!! *)

let label_gen = 
  let id = ref 0 in
  function l -> 
	   id := !id + 1;
	   "dynamic"^(string_of_int !id) ^ scope_str ^l

let rec add_dot container object_container env (nodes,arcs) (t,l) =
  let full_label, l_node = match t with
    | PVariable x -> 
       begin
	 try let full_l = (SMap.find x env) ^ scope_str ^ l in
	     full_l, (full_l, l)
	 with Not_found -> failwith (x ^ " not found in env")
       end
    | _ -> let l = label_gen l in 
	   l, (l,l)
  in		       
  add_p_term_to_graph container object_container env (l_node :: nodes,  Uses (container, full_label) :: arcs) t


and  add_p_term_to_graph container object_container env graph t =
  let add_term = add_p_term_to_graph container object_container env in
  match t with
  | PVariable _ -> graph
  | PObject method_list ->
     List.fold_left (add_method_to_graph add_p_term_to_graph container object_container env) graph method_list
		    
  | PSelection (t,l) -> add_dot container object_container env graph (t,l)
			      
  | PUpdate (t, l, m) ->
     let graph = add_method_def add_p_term_to_graph container (fun _ -> object_container) env graph m in
     add_dot container object_container env graph (t,l)
			   
  | PConst _ -> graph
  | PMathExpr (_, t1, t2) -> add_term (add_term graph t1) t2
  | PLambda (b, t) -> 
     let (nodes,arcs) = graph 
     and fullb = container ^ scope_str ^ b in
     let b_node = (fullb, b) in
     add_term (b_node :: nodes, Contains (container, fullb) :: arcs) t
  | PApply (t1, t2) -> add_term (add_term graph t1) t2

let ag_of_p_program (prog_defs, t) = 
  let r = "root" in
  let r_node = (r,r) in
  let init_graph = [r_node], [] in
  
  let init_env, init_graph = List.fold_left 
	    begin fun (env, (nodes, arcs)) (n,t) -> 
		  let r_dot_n =  r ^ scope_str ^ n in 
		  let env = SMap.add n r_dot_n env in
		  env, ((r_dot_n, n) :: nodes, Contains(r, r_dot_n) :: arcs)
	    end (SMap.empty, init_graph) prog_defs
  in
  let g = List.fold_left (fun g (n,t) -> add_p_term_to_graph (r ^ scope_str ^ n) (r ^ scope_str ^ n) init_env g t) init_graph prog_defs in
  add_p_term_to_graph r "no_containing_object" init_env g t
