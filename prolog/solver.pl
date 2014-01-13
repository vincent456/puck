


'contains*'(X,X, Nodes, _) :- member(node(X, _, _), Nodes).
'contains*'(X,Z, Nodes, Contains) :- member(edge(contains,X,Y), Contains), 'contains*'(Y,Z, Nodes, Contains).

friend(User, Usee, graph(Nodes, Contains, _), Constraints):-
    member(isFriendOf(UserAncestor, UseeAncestor), Constraints),
    'contains*'(UserAncestor, User, Nodes, Contains),
    'contains*'(UseeAncestor, Usee, Nodes, Contains). 

%%
%%abstract(+LocalName, +UseeType,+ParentUid, -Abstraction)
%%

abstract(LocalName, UseeType, ParentUid, node(AbsUid, UseeType, AbsName)):-
    atomic_concat('abstract_',LocalName, AbsName),
    atomic_concat(ParentUid, '.', Pdot),
    atomic_concat(Pdot, AbsName, AbsUid).

%%
%%fold(+Graph, +Violation, +RuleViolated, +Abstractions,
%%         -GraphOut, -Abstractions).
%%

%if alreadey abstracted, do not recreate it
fold(graph(N,C,U), edge(uses, User, Usee), _, Abstractions, 
     graph(N,C,[edge(uses, User, AbsUsee) | U]), Abstractions):- 
    member(abstract(Usee,AbsUsee), Abstractions).

fold(graph(N, C, UsesWithoutViolation),
     edge(uses, User,Usee), hideFrom(Hidee, _), Abstractions, 
     graph([node(AbsUid, AbsType,AbsName) | N], 
	   [ edge(contains,HideeParent, AbsUid)| C],
	   [ edge(uses, User, AbsUid)| [edge(uses, AbsUid, Usee)| UsesWithoutViolation]] ), 
     [abstract(Usee, AbsUid)| Abstractions]) :- 
    \+member(abstract(Usee, _), Abstractions),
    member(node(Usee,UseeType,Nick), N), member(edge(contains,HideeParent, Hidee), C),
    abstract(Nick, UseeType, HideeParent, node(AbsUid, AbsType, AbsName)).

%%
%%solve(+GraphIn, +Constraints, -GraphOut)
%%
solve(GraphIn, Constraints, GraphOut):- solve(GraphIn, Constraints, [], GraphOut, _).

%%
%%solve(+GraphIn, +Constraints, +Abstractions, -GraphOut, -GraphOut)
%%
solve(graph(N,C,U), Constraints, Abstractions, GraphOut,AbsOut) :-
    violation(graph(N, C, U) , Constraints, Violation, ViolatedConstraint, UsesWithoutViolation),
    fold(graph(N, C, UsesWithoutViolation), Violation, ViolatedConstraint, 
	     Abstractions, GraphO1,AbsOut1), 
    solve(GraphO1, Constraints, AbsOut1, GraphOut, AbsOut).

solve(Graph, Constraints, Abs, Graph, Abs):- \+ violation(Graph, Constraints, _, _,_).

%violation(+Uses, +Contains, +Constraints, -Violation, -Cause)
violation(graph(Nodes, Contains, Uses), Constraints, edge(uses, User,Usee), hideFrom(Hidee, Interloper), OtherUses):-
    member(hideFrom(Hidee, Interloper),Constraints),
    select(edge(uses, User,Usee), Uses, OtherUses), 
    \+friend(User, Usee, graph(Nodes, Contains, Uses), Constraints),
    'contains*'(Hidee, Usee, Nodes, Contains), 'contains*'(Interloper,User, Nodes, Contains).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% writing a graph.


pl2dot(File, graph(N,C,U)):- 
    tell(File),
    writeln('digraph G {'), %linux dotty parser doesn't accept anonym graph %'
    nodes2dot(N),
    edges2dot(C),
    edges2dot(U),
    writeln('}'),
    told.

%% ru2dot(File):- 		pl2dot(File, redUses),!.
%% ru2dot		:- 		ru2dot('ru.dot').

pl2dot(Graph)	:- pl2dot('graph.dot', Graph).

% writes all the nodes to the current output in the .dot format
nodes2dot(Ns):- maplist(writelnNode, Ns).

writeQuoted(Node):-
	write('"'),
	write(Node),
	write('"').
	
writelnNode(node(Uid, Kind, Name)) :- 
	assertz(written(Uid)),
	writeQuoted(Uid),
	write(' [ label = '),
	writeQuoted(Name),
	write(', shape = '),
	kind2shape(Kind, Shape),
	write(Shape),
	write(', style = filled, fillcolor = '),
	nodeKind2fillColor(Kind, Color),
	write(Color),
	writeln(' ];').


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

writelnEdge(edge(Kind, Source, Target)) :- 
	assertz(written(edge(Kind,Source,Target))),
	writeQuoted(Source),
	write(' -> '),
	writeQuoted(Target),
	write(' [ style = '),
	kind2style(Kind, Style),
	write(Style),

	%% write(', color = '),
	%% status2Color(Status, Color),
	%% write(Color),

	%% write(', penwidth = '),
	%% status2thickness(Status, T),
	%% write(T),
	
	write(', arrowhead = '),
	kind2headStyle(Kind, HeadStyle),
	write(HeadStyle),
	writeln(' ];').

%% status2Color(correct, black).
%% status2Color(incorrect, red).

%% status2thickness(correct, 1).
%% status2thickness(incorrect, 5).


kind2style(isa, solid).
kind2style(contains, dashed).
kind2style(virtualContains, dashed).
kind2style(uses, bold).

kind2headStyle(isa, empty).
kind2headStyle(contains, 'open').
kind2headStyle(virtualContains, 'open').
kind2headStyle(uses, normal).
