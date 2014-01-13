
module type StrFormats =
  sig
    val node_str : string
    val contains_str : string
    val uses_str : string
    val graph_str : string
  end


module Make = 
  functor(Label : Node.Label) ->
	 struct

	   open Access_graph
	   open Format

	   let fprint_edge str fmt (uid1, uid2) =
	     fprintf fmt (Scanf.format_from_string str "%s%s") uid1 uid2

	   module Printer =
	     functor(StrF : StrFormats) ->
		    struct 
		      open StrF
			     
		      let fprint_node fmt n =
			fprintf fmt (Scanf.format_from_string node_str "%s%s") (Label.uid n) (Label.nickname n)

		      let fprint_contains = fprint_edge contains_str
							
		      let fprint_uses = fprint_edge uses_str
						    
		      (* let fprint_red_uses  *)

		      let fprint_ag fmt g =
			let rec f fmt n =
			  fprint_node fmt n.value;
			  let nuid1 = Label.uid n.value in
			  List.iter (fun n2 -> 
				     f fmt n2;
				     fprint_contains fmt (nuid1, Label.uid n2.value) ) n.children;
			  List.iter (fun n2 ->
				     fprint_uses fmt (nuid1, Label.uid n2.value)) n.uses
			in
			fprintf fmt (Scanf.format_from_string graph_str "%a") f g 

		      let print g =  fprint_ag std_formatter g

		    end
		      
		      
	   module Prolog = 
	     Printer(struct
		      let node_str = "node('%s','%s').@\n"
		      let contains_str = "contains('%s', '%s').@\n"
		      let uses_str = "uses('%s', '%s').@\n"

		      let graph_str = "%a"
		    end)

	   module Dot = 
	     Printer(struct
		      let node_str = "\"%s\" [ label =\"%s\", shape = ellipse, style = filled, fillcolor = \"#FFFFFF\" ];@\n"
		      let contains_str = "/*contains*/ \"%s\" -> \"%s\" [style = dashed, color = black, penwidth = 1, arrowhead = open ];@\n"
		      let uses_str = "/*uses*/ \"%s\" -> \"%s\" [style = bold, color = black, penwidth = 1, arrowhead = normal ];@\n"

		      let graph_str = "digraph G {@[%a@]}"

		    (* let red_uses =  "/*uses - red */ \"%s\" -> \"%s\" [style = bold, color = red, penwidth = 5, arrowhead = normal ];@\n" *)
		    end)

	 end
