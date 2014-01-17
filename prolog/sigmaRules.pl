:-module(javaRules, 
       [can_contain/2,
	abstract/6,
	abstract_type/2]).
:-use_module(graph).

can_contain(object, object).
can_contain(object,method).

abstract_type(object, object).

abstract(Node, GraphIn, AbstractAssoc, Abs, G3, AbstractAssocOut):-
    abstract_node(Node, GraphIn, AbstractAssoc, Abs, G2, AbstractAssocOut),
    id_of_node(AbsId, Abs), id_of_node(NodeId, Node),
    put_uses(AbsId, NodeId, G2, G3).
