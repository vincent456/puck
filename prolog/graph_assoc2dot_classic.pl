:-module(pl2dot, 
	 [pl2dot/2,
	  pl2dot/3]).

pl2dot(File, Graph) :- pl2dot(File, Graph, []).

pl2dot(File, (Ns, Us, _), V):- 
    tell(File),
    writeln('digraph G {'), %linux dotty parser doesn't accept anonym graph %'
    nodes2dot(Ns),
    edges2dot(Us),
    violations2dot(V),
    writeln('}'),
    told.

nodes2dot(Ns):- 
    map_assoc(writelnNode, Ns).

writeQuoted(Node):-
    write('"'),
    write(Node),
    write('"').

writelnContains(Cee, Cer, Cer):- writelnEdge((contains, Cer, Cee)).
	
writelnNode((Uid, (Kind, Name,_), (Cer, Cees), _)) :- 
    write(Uid),
    write(' [ label = '),
    write('"'),
    write(Name), write(' ('),write(Uid),
    write(')"'),
    write(', fontcolor = '),
    container2fontcolor(Cer, FColor),
    write(FColor),
    write(', shape = '),
    kind2shape(Kind, Shape),
    write(Shape),
    write(', style = filled, fillcolor = '),
    nodeKind2fillColor(Kind, Color),
    write(Color),
    writeln(' ];'),
    foldl(writelnContains, Cees, Uid, _).


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

edges2dot(Es):- maplist(writelnEdge,Es).
violations2dot(Vs):- maplist(writelnViolation, Vs).

writelnEdge(Edge) :- writelnEdgeStatus(Edge, correct).
writelnViolation(Edge) :- writelnEdgeStatus(Edge, incorrect).

writelnEdgeStatus((Kind, Source, Target), Status) :- 
	write(Source),
	write(' -> '),
	write(Target),
	write(' [ style = '),
	kind2style(Kind, Style),
	write(Style),

	write(', color = '),
	status2Color(Status, Color),
	write(Color),

	write(', penwidth = '),
	status2thickness(Status, T),
	write(T),
	
	write(', arrowhead = '),
	kind2headStyle(Kind, HeadStyle),
	write(HeadStyle),
	writeln(' ];').

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
