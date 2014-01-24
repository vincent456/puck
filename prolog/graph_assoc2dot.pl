:-module(pl2dot, 
	 [pl2dot/2,
	  pl2dot/3]).
:-use_module(graph).

pl2dot(File, Graph) :- pl2dot(File, Graph, []).

pl2dot(File, G, V):- 
    tell(File),
    writeln('digraph G {'), %linux dotty parser doesn't accept anonym graph %'
    writeln('rankdir=LR;'),
    nodes2dot(G),
    edges2dot(G),
    violations2dot(V, G),
    writeln('}'),
    told.

nodes2dot(G):- 
    get_roots(Ids, G),
    foldl(writeln_node, Ids, G, G).

writeQuoted(Node):-
    write('"'),
    write(Node),
    write('"').

writeln_contains(Cee, (Cer, G), (Cer, G)):- writeln_edge((contains, Cer, Cee), G, G).

hook(interface).
hook(class).

hooked(method).
hooked(attribute).
hooked(constructor).

write_label(Id, (Nb, G), (Nb1, G)):-
    Nb1 is Nb +1,
    write(' | <f'), write(Nb),write('> '), 
    get_node(Id, (Id, (_,Name,_),_,_), G), write(Name), write(' ('),write(Id),write(')').

writeln_node(Id, G, G) :- 
    get_node(Id, (Id, (Kind, Name,_), (_, Cees), _), G),
    write(Id),
    write(' [ label = '),
    write('"<f0> '), 
    write(Name), write(' ('),write(Id),write(')'),
    (hook(Kind) *-> foldl(write_label, Cees, (1, G), (_, G)); true),
    writeln('"'),
    writeln(', shape = "record", style = filled, fillcolor = '),
    nodeKind2fillColor(Kind, Color),
    write(Color),
    writeln(' ];'),
    ( (hook(Kind); hooked(Kind)) *-> true; 
      foldl(writeln_node, Cees, G, G),
      foldl(writeln_contains, Cees, (Id, G), (_, G))).
	

nodeKind2fillColor(virtualScope,'"#33FF33"').%Green
nodeKind2fillColor(package,'"#FF9933"').%Orange
nodeKind2fillColor(interface,'"#FFFF99"').%Light yellow
nodeKind2fillColor(class,'"#FFFF33"').%Yellow

nodeKind2fillColor(constructor,'"#FFFF33"').%yellow
nodeKind2fillColor(method,'"#FFFFFF"').%White
nodeKind2fillColor(attribute,'"#FFFFFF"').%White
nodeKind2fillColor(stringLiteral,'"#CCFFCC"').%Very light green


edges2dot((Ns, Us, Nb)):- foldl(writeln_edge, Us, (Ns, Us, Nb), (Ns, Us, Nb)).
violations2dot(Vs, G):- foldl(writeln_violation, Vs, G, G).

writeln_edge(Edge,G,G) :- writeln_edge_status(Edge, G, correct).
writeln_violation(Edge,G,G) :- writeln_edge_status(Edge, G, incorrect).

record_top(X):-hook(X).
record_top(package).

write_dot_id(Id, G):-
    get_node(Id, N, G), type_of_node(T, N), 
    (record_top(T) *-> write_dot_id_aux(Id, 0);
     container_of_node(CerId, N),
     get_node(CerId, Cer, G), containees_of_node(Cees,Cer),
     nth1(Nth, Cees, Id), write_dot_id_aux(CerId, Nth)).

write_dot_id_aux(Src, Nth):-write(Src), write(':f'), write(Nth).

writeln_edge_status((Kind, Source, Target), G, Status) :- 
    write_dot_id(Source, G),
    write(' -> '),
    write_dot_id(Target, G),

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
