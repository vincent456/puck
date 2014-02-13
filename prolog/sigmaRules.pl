:-module(sigmaRules, 
       [can_contain_kind/2,
	abstract/6,
	abstract_kind/2,
	violations_node_type_priority/1,
	
	hook/1,
	hooked/1,
	record_top/1,
	node_kind_to_fill_color/2]).
:-use_module(graph).

can_contain_kind(object, object).
can_contain_kind(object, method).

abstract_kind(object, object).

abstract(Node, GraphIn, AbstractAssoc, Abs, G3, AbstractAssocOut):-
    abstract_node(Node, GraphIn, AbstractAssoc, Abs, G2, AbstractAssocOut),
    id_of_node(AbsId, Abs), id_of_node(NodeId, Node),
    put_uses(AbsId, NodeId, G2, G3).


violations_node_type_priority([]).

%%%% printing predicates

hook(_):-false.
hooked(_):-false.

record_top(object).

node_kind_to_fill_color(object,'"#FFFFFF"').%White
node_kind_to_fill_color(method,'"#FFFFFF"').%White
