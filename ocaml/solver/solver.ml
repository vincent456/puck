module Make = 
  functor(Label: Node.Label) ->
	 struct
	   open Access_graph

	   exception Found

	   let find graph label =
	     let v = ref None in
	     try
	       iter (function n ->
			      if Label.equal n.value label then 
				begin
				  v := Some n;
				  raise Found
				end) graph;
	       raise Not_found
	     with Found -> 
	       begin
		 match !v with
		 | Some v -> v
		 | None -> assert false
	       end

	   let find_child g label = List.find (function n -> Label.equal n.value label) g.children

	   let contains g label =
	     try ignore (find_child g label); true
	     with Not_found -> false

	   let tcontains g label =
	     try ignore (find g label); true
	     with Not_found -> false


	   let replace old_node new_node nlist =
	     let rec f acc = function
	       | [] -> (* List.rev *) acc
	       | hd :: tl -> 
		  f ((if Label.equal hd.value old_node.value then new_node
		      else hd) :: acc) tl
	     in
	     f [] nlist

	   let isContainedBy n1 n2 =
	     let rec f n =
	       match n.parent with
	       | None -> false
	       | Some p -> Label.equal p.value n2.value || f p
	     in
	     f n1


	   (* n can use n2 *)
	   let rec isFriendOf n n2 =
	     List.exists (fun f -> Label.equal f.value n2.value || isContainedBy n2 f) n.friends
	     || parentIsFriendOf n n2 
	   and parentIsFriendOf n n2 =
	     match n.parent with
	     | None -> false
	     | Some p -> isFriendOf p n2



	   (* parcours en profondeur du graph et collecte de toutes les violations*)    
	   let find_violations g =
	     let rec f hiddenFrom violations  usee =
	       let hiddenFrom = usee.interlopers @ hiddenFrom in
	       let violations = List.fold_left begin fun vs user -> 
						     List.fold_left begin fun vs interloper -> 
									  if (Label.equal user.value interloper.value || isContainedBy user interloper)
									     && not (isFriendOf user usee) then (user, usee) :: vs
									  else vs
								    end vs hiddenFrom 
					       end violations usee.used_by
	       in List.fold_left (f hiddenFrom) violations usee.children
	     in f [] [] g


					    
	   exception Ag_transformation_exn of string

	   exception Break

	   let find_and_repare g =
	     let abs_list = ref [] in
	     
	     let rec f interlopers usee =
	       
	       let rec add_interlopers acc = function 
		 | [] -> acc
		 | i :: tl -> add_interlopers ((usee, i) :: acc) tl
	       (*on mémorise sur qui porte la contrainte :
		* lorsqu'on abstrait usee, on place l'abstraction de sorte qu'elle ne viole pas également la contrainte,
		* on place donc l'abstraction comme "sibling" du noeud caché
		*)
	       in 
	       let hiddenFrom = add_interlopers interlopers usee.interlopers in
	       
	       let abstraction = ref None in

	       (* create an abstraction if there isn't any or if the new host contains the old one *)
	       let abstract host = 
		 
		 let create () = create_node (Label.abstract usee.value host.value) (Some host) [] [usee] [] in
		 
		 match !abstraction with
		 | None -> abstraction :=  Some (create () )
		 | Some abs -> begin 
			       match abs.parent with
			       | Some p -> 
				  if not (p.value = host.value) && (isContainedBy p host) then
				    abstraction :=  Some (create () )
			       |_ -> assert false
			     end
	       in
	       
	       let users, abs_users =
		 List.fold_left begin fun (acc_users, acc_abs_users) user -> 
				      try 
					List.iter begin fun (hidden, interloper) -> 
							if ((user.value = interloper.value) || (isContainedBy user interloper))  
							   && not (isFriendOf user usee) then
							  match hidden.parent with
							  | Some p -> abstract p; raise Break
							  | _ -> raise (Ag_transformation_exn ("No host for abstraction of " ^ (Label.string_of_label usee.value)))
						  end hiddenFrom;
					(user :: acc_users, acc_abs_users)
				      with Break -> (acc_users, user :: acc_abs_users)
						      
				end ([],[]) usee.used_by 
	       in
	       usee.used_by <- users;
	       
	       (* "close abstraction" *)
	       (match !abstraction with
		| None -> ()
		| Some abs -> begin
			      abs_list := abs :: !abs_list;
			      match abs.parent with
			      | Some p -> 
				 p.children <- abs :: p.children;
				 usee.used_by <- abs :: usee.used_by;
				 List.iter (fun u -> u.uses <- replace usee abs u.uses) abs_users;
				 abs.used_by <- abs_users
						  
			      | None -> assert false
			    end);
	       
	       List.iter (f hiddenFrom) usee.children
	     in f [] g;
		!abs_list
		 
end
