
type 'a m_node_t = 
    { value : 'a; 
      mutable parent : 'a m_node_t option;
      mutable children : 'a m_node_t list;
      mutable uses : 'a m_node_t list;
      mutable used_by: 'a m_node_t list;
			  
      mutable interlopers: 'a m_node_t list; (* this is hidden from members of this.interlopers *)
      mutable friends: 'a m_node_t list  (* this can uses members of this.friends *)
    }

(* type 'a rule_t = 'a constraint_t -> 'a t -> 'a t *)

let create_node v p c u ub = 
  { value = v;
    parent = p;
    children = c;
    uses = u;
    used_by = ub;
    interlopers = [];
    friends = []
  }

let create_empty_node v = create_node v None [] [] []

let rec iter f g =
  f g.value; List.iter (iter f) g.children

let rec fold f acc g =
  List.fold_left (fold f) (f acc g.value) g.children
