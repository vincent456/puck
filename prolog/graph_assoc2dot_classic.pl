:-module(graph_assoc2dot, 
	 [pl2dot/2,
	  pl2dot/3]).
:-use_module(graph).

pl2dot(File, Graph) :- pl2dot(File, Graph, []).

pl2dot(File, (Ns,_,_), V):- 
    tell(File),
    writeln('digraph G {'), %linux dotty parser doesn't accept anonym graph %'
    nodes2dot(Ns),
    violations2dot(V),
    writeln('}'),
    told.

nodes2dot(Ns):- 
    map_assoc(writeln_node, Ns).

writeln_contains(Cer, Cee):- writeln_edge((contains, Cer, Cee)).
writeln_uses(Usee, User):-writeln_edge((uses, User, Usee)).
writeln_isa(Sub, Super):-writeln_edge((isa, Sub, Super)).

writeln_node(Node) :- 
    id_of_node(Uid, Node),
    identity_of_node((Kind, Name, _), Node),
    container_of_node(Cer, Node),
    containees_of_node(Cees, Node),
    users_of_node(Users, Node),
    super_types_of_node(Supers, Node),

    container2fontcolor(Cer, FColor),
    kind2shape(Kind, Shape),
    nodeKind2fillColor(Kind, Color),
    
    atomic_list_concat([Uid, ' [ label = "', Name, ' (', Uid, 
        ')", fontcolor = ', FColor, ', shape = ', Shape, 
        ', style = filled, fillcolor = ', Color, ' ];'], Str),
    write(Str),
    maplist(call(writeln_contains(Uid)), Cees),
    maplist(call(writeln_uses(Uid)), Users),
    maplist(call(writeln_isa(Uid)), Supers).


container2fontcolor(no_parent, red).
container2fontcolor(X, black):- number(X).

kind2shape(object, ellipse).

kind2shape(class,rectangle).
kind2shape(method,diamond).
kind2shape(attribute,ellipse).
kind2shape(package,trapezium).
kind2shape(virtualScope,invtriangle).
kind2shape(interface,parallelogram).
kind2shape(constructor,diamond).
kind2shape(stringLiteral,note).

nodeKind2fillColor(object,'"#FFFFFF"').%White

nodeKind2fillColor(virtualScope,'"#33FF33"').%Green
nodeKind2fillColor(package,'"#FF9933"').%Orange
nodeKind2fillColor(interface,'"#FFFF99"').%Light yellow
nodeKind2fillColor(class,'"#FFFF33"').%Yellow
nodeKind2fillColor(constructor,'"#FFFF33"').%yellow
nodeKind2fillColor(method,'"#FFFFFF"').%White
nodeKind2fillColor(attribute,'"#FFFFFF"').%White
nodeKind2fillColor(stringLiteral,'"#CCFFCC"').%Very light green

violations2dot(Vs):- maplist(writeln_violation, Vs).

writeln_edge(Edge) :- writeln_edge_status(Edge, correct).
writeln_violation(Edge) :- writeln_edge_status(Edge, incorrect).

writeln_edge_status((Kind, Source, Target), Status) :- 
	kind2style(Kind, Style),
    status2Color(Status, Color),
    status2thickness(Status, T),
    
    kind2headStyle(Kind, HeadStyle),
    atomic_list_concat([Source, ' -> ', Target, ' [ style = ',
	   Style, ', color = ', Color, ', penwidth = ', T, ', arrowhead = ',
	   HeadStyle, ' ];'], Str),
    write(Str).

status2Color(correct, black).
status2Color(incorrect, red).

status2thickness(correct, 1).
status2thickness(incorrect, 5).


kind2style(isa, solid).
kind2style(contains, dashed).
kind2style(virtualContains, dashed).
kind2style(uses, bold).

kind2headStyle(isa, empty).
kind2headStyle(contains, 'open').
kind2headStyle(virtualContains, 'open').
kind2headStyle(uses, normal).
