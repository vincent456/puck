
open Access_graph
open Parsed_terms

type named_element_t =
  | Method of p_method_t
  | Term of p_term_t


let split string char =
  let rec f acc n =
    try
      let n0 = String.rindex_from string n char in
      let acc = if n - n0 = 0 then acc (* if string have two consecutive 'char' or finish with char *)
		else (String.sub string (n0 + 1) (n - n0)) :: acc
      in f acc (n0 - 1)
    with Not_found -> 
      if n = -1 then acc (* if string begins with char *)
      else (String.sub string 0 (n + 1)) :: acc
  in f [] ( (String.length string)-1 )

let named_element uname defs =
  let rec f t un =
    match un with
    | [] -> Term t
    | hd :: tl -> 
       begin
	 match t with
	 | PObject ms -> g (List.assoc hd ms) tl
	    
	 | PSelection t, _ 
	 | PLambda _, t ->  f t un
	 
	 | PUpdate t, _, m -> 
	    begin try f t un with 
		  | Not_found -> g m und
	    end

	 | PMathExpr _, t1,t2
	 | PApply t1, t2 ->
	    begin try f t1 un with 
		  | Not_found -> f t2 un
	    end

	 | _ -> raise Not_found
       end
    and g (b,body) un =
      match un with
      | [] -> Method (b,body)
      | _ -> f body tl
      
  in
  match uname with
  | [] -> raise Not_found
  | hd :: tl -> List.assoc hd defs
  

let refactor p_program constraints =
  let (nodes, edges) = List_ag_constructor.ag_of_p_program p_program in
  let ag = Ag_of_list_ag.ag_of_list_ag (nodes, edges, constraints) in

