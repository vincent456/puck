:-module(javaRules, 
	 [can_contain_type/2,
	  abstract/6,
	  abstract_type/2]).
:-use_module(graph).

can_contain_type(package, package).
can_contain_type(package, class).
can_contain_type(package, interface).
can_contain_type(class, class).
can_contain_type(class, method).
can_contain_type(interface, method).
%can_contain_type(method, param).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstract_type(package, package). %% is it ?
abstract_type(interface,interface).
abstract_type(class, interface).
abstract_type(method, method). %% methodImpl methodSig ??

abstract_method_list([], _, Graph, AbsAssocs, Graph, AbsAssocs).
abstract_method_list([M|ML], InterfaceId, Graph, AbsAssocs, NewGraph, NewAbsAssocs):-
    abstract_node(M, Graph, AbsAssocs, Abs, G2, AA2),
    id_of_node(AbsId, Abs),
    put_contains(InterfaceId, AbsId, G2, G3),
    abstract_method_list(ML, InterfaceId, G3, AA2, NewGraph, NewAbsAssocs).
    
%%%%%%%%%%%%%
abstract(Node, GraphIn, AbstractAssoc, Abs, G3, AbstractAssocOut):-
    type_of_node(Type, Node), id_of_node(NodeId, Node),
    \+abstract_type(Type, interface),
    abstract_node(Node, GraphIn, AbstractAssoc, Abs, G2, AbstractAssocOut),
    id_of_node(AbsId, Abs), put_uses(AbsId, NodeId, G2, G3).

abstract(Node, GraphIn, AbstractAssoc, Abs, G4, AbstractAssocOut):-
    type_of_node(Type, Node), id_of_node(NodeId, Node),
    abstract_type(Type, interface),
	    
    abstract_node(Node, GraphIn, AbstractAssoc, Abs, G2, AA2),
    
    id_of_node(AbsId, Abs),  put_uses(NodeId, AbsId, G2, G3),
    
    findall(M, 
	    (contains(NodeId, MId, GraphIn),
	     type_of_node(method, M),
	     get_node(MId, M, GraphIn)), 
	    Methods),
    
    abstract_method_list(Methods, AbsId, G3, AA2, G4, AbstractAssocOut).
