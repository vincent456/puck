
module Make = 
  functor(Label : Node.Label) ->
struct

  open List_access_graph
  open Access_graph
  module SMap = Utils.SMap

  (*/!\ NO TESTS of WELLFORMEDNESS is performed !! /!\*) 

  let node_index : Label.t m_node_t SMap.t ref = ref SMap.empty

  (* first all nodes are added, then we add the uses and contains*)
  let add_node label =
    node_index := SMap.add (Label.uid label) (create_empty_node label) !node_index

  let find2 (uid1, uid2) = SMap.find uid1 !node_index, SMap.find uid2 !node_index
								 
  let add_edge = function
    | Uses (uid1, uid2) ->
       let n1,n2 = find2 (uid1,uid2) in
       n1.uses <- n2 :: n1.uses;
       n2.used_by <- n1 :: n2.used_by
			     
    | Contains (uid1, uid2) ->
       let n1,n2 = find2 (uid1,uid2) in
       n1.children <- n2 :: n1.children;
       n2.parent <- Some n1

  let add_constraint = function
    | HideFrom (uid1, uid2) ->
       let n1,n2 = find2 (uid1,uid2) in
       n1.interlopers <- n2 :: n1.interlopers

    | IsFriendOf (uid1, uid2) ->
       let n1,n2 = find2 (uid1,uid2) in
       n1.friends <- n2 :: n1.friends

  let find_roots () = 
    SMap.fold (fun _ n acc ->
	       match n.parent with
	       | None -> n :: acc
	       | _ -> acc) !node_index []

  let ag_of_list_ag (nodes, edges, constraints) =
    node_index := SMap.empty;
    List.iter add_node nodes;
    List.iter add_edge edges;
    List.iter add_constraint constraints;
    find_roots ()

end   
