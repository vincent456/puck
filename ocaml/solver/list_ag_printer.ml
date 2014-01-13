
open List_access_graph
open Format

(* let print_node fmt (uid, nick) = fprintf fmt "node('%s','%s').@\n" uid nick *)

(* let print_edge fmt = function *)
(*   | Uses (uid1, uid2) -> fprintf fmt "uses('%s', '%s').@\n" uid1 uid2 *)
(*   | Contains (uid1, uid2) -> fprintf fmt "contains('%s', '%s').@\n" uid1 uid2 *)

let print_node fmt (uid, nick) = fprintf fmt "node('%s',object, '%s').@\n" uid nick

let print_edge fmt = function
  | Uses (uid1, uid2) -> fprintf fmt "edge(uses, '%s', '%s').@\n" uid1 uid2
  | Contains (uid1, uid2) -> fprintf fmt "edge(contains, '%s', '%s').@\n" uid1 uid2

let print_constraint fmt = function
  | HideFrom (uid1, uid2) -> fprintf fmt "hiddenFrom('%s', '%s').@\n" uid1 uid2
  | IsFriendOf (uid1, uid2) -> fprintf fmt "isFriendOf('%s', '%s').@\n" uid1 uid2
				       

let fprint_ag fmt (nodes, edges, constraints) =
  List.iter (print_node fmt) nodes;
  List.iter (print_edge fmt) edges;
  List.iter (print_constraint fmt) constraints

let print_ag = printf "%a" fprint_ag

let eprint_ag = eprintf "%a" fprint_ag
    
