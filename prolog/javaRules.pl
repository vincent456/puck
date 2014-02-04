:-module(javaRules, 
	 [can_contain_type/2,
	  abstract/5,
	  abstract_type/2,
	  violations_node_type_priority/1,
	  %%%% printing predicates
	  subgraph/1,
	  hook/1,
	  hooked/1,
	  record_top/1,
	  node_kind_to_fill_color/2]).
:-use_module(graph).

can_contain_type(package, package).
can_contain_type(package, class).
can_contain_type(package, interface).
can_contain_type(class, class).
%can_contain_type(class, interface).
can_contain_type(class, method).
can_contain_type(class, constructor).
can_contain_type(class, attribute).
can_contain_type(interface, method). 
%can_contain_type(method, param).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstract_type(package, package). %% is it ?
abstract_type(interface,interface).
abstract_type(class, interface).
abstract_type(attribute, method).
abstract_type(method, method). %% methodImpl methodSig ??
abstract_type(constructor, method).

abstract_method_list([], _, Graph, Graph).
abstract_method_list([M|ML], InterfaceId, Graph, NewGraph):-
    abstract_node(M, Graph, Abs, G2),
    id_of_node(AbsId, Abs),
    put_contains(InterfaceId, AbsId, G2, G3),
    abstract_method_list(ML, InterfaceId, G3, NewGraph).
    
%%%%%%%%%%%%%
%% (+Node, +Graph, +AbsAssoc
%% -Abs, -UseeId, -NewGraph, - NewAbsAssoc
abstract(Node, GraphIn, Abs, NodeId, G3):-
    type_of_node(Type, Node), id_of_node(NodeId, Node),
    \+abstract_type(Type, interface),
    abstract_node(Node, GraphIn, Abs, G2),
    id_of_node(AbsId, Abs), put_uses(AbsId, NodeId, G2, G3).

abstract(Node, GraphIn, Abs, AbsId, G4):-
    type_of_node(Type, Node), id_of_node(NodeId, Node),
    abstract_type(Type, interface),
	    
    abstract_node(Node, GraphIn, Abs, G2),
    
    id_of_node(AbsId, Abs),  put_uses(NodeId, AbsId, G2, G3),
    
    findall(M, 
	    (contains(NodeId, MId, GraphIn),
	     type_of_node(method, M),
	     get_node(MId, M, GraphIn)), 
	    Methods),
    
    abstract_method_list(Methods, AbsId, G3, G4).

%%%%%

violations_node_type_priority([class, interface]).

%%%% printing predicates

subgraph(package).

hook(interface).
hook(class).

hooked(method).
hooked(attribute).
hooked(constructor).

record_top(X):-hook(X).
record_top(package).


node_kind_to_fill_color(virtualScope,'"#33FF33"').%Green
node_kind_to_fill_color(package,'"#FF9933"').%Orange
node_kind_to_fill_color(interface,'"#FFFF99"').%Light yellow
node_kind_to_fill_color(class,'"#FFFF33"').%Yellow

node_kind_to_fill_color(constructor,'"#FFFF33"').%yellow
node_kind_to_fill_color(method,'"#FFFFFF"').%White
node_kind_to_fill_color(attribute,'"#FFFFFF"').%White
node_kind_to_fill_color(stringLiteral,'"#CCFFCC"').%Very light green
