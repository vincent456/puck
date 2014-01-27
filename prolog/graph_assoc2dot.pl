:-module(pl2dot, 
	 [pl2dot/2,
	  pl2dot/3]).
:-use_module(graph).

pl2dot(File, Graph) :- pl2dot(File, Graph, []).

pl2dot(File, G, V):- 
    open(File, write, Stream, []),
    write(Stream, 'digraph G {\n'), %linux dotty parser doesn't accept anonym graph %'
    write(Stream, 'rankdir=LR; ranksep=2;\n'),
    nodes2dot(G, Stream),
    edges2dot(G, Stream),
    violations2dot(V, G, Stream),
    write(Stream, '}\n'),
    close(Stream).

nodes2dot(G, Stream):- 
    get_roots(Ids, G),
    foldl(writeln_node, Ids, (G, Stream), (G, Stream)).

writeln_contains(Cee, (Cer, G, S), (Cer, G, S)):- writeln_edge((contains, Cer, Cee), (G,S), (G,S)).


write_label(Id, (Nb, G, S), (Nb1, G, S)):-
    Nb1 is Nb +1,
    write(S, ' | <f'), write(S, Nb),write(S, '> '), 
    get_node(Id, (Id, (_,Name,_),_,_), G), write(S, Name), write(S, ' ('),write(S, Id),write(S, ')').

writeln_node(Id, (G, S), (G, S)) :- 
    get_node(Id, (Id, (Kind, Name,_), (_, Cees), _), G),
    write(S, Id),
    write(S, ' [ label = '),
    write(S, '"<f0> '), 
    write(S, Name), write(S, ' ('),write(S, Id),write(S, ')'),
    (hook(Kind) *-> foldl(write_label, Cees, (1, G, S), _); true),
    write(S,'"\n'),
    write(S, ', shape = "record", style = filled, fillcolor = '),
    node_kind_to_fill_color(Kind, Color),
    write(S, Color),
    write(S, ' ];\n'),
    ((hook(Kind); hooked(Kind)) *-> true; 
      foldl(writeln_node, Cees, (G, S), (G, S)),
      foldl(writeln_contains, Cees, (Id, G, S), (_, G, S))).
	



edges2dot((Ns, Us, Nb), S):- foldl(writeln_edge, Us, ((Ns, Us, Nb),S), _).
violations2dot(Vs, G, S):- foldl(writeln_violation, Vs, (G,S), _).

writeln_edge(Edge,(G,S),(G,S)) :- writeln_edge_status(Edge, G, correct, S).
writeln_violation(Edge,(G,S),(G,S)) :- writeln_edge_status(Edge, G, incorrect, S).


write_dot_id(Id, G, S):-
    get_node(Id, N, G), type_of_node(T, N), 
    (record_top(T) *-> write_dot_id_aux(Id, 0,S);
     container_of_node(CerId, N),
     get_node(CerId, Cer, G), containees_of_node(Cees,Cer),
     nth1(Nth, Cees, Id), write_dot_id_aux(CerId, Nth, S)).

write_dot_id_aux(Src, Nth, Stream):-write(Stream, Src), write(Stream, ':f'), write(Stream, Nth).

writeln_edge_status((Kind, Source, Target), G, Status, Stream) :- 
    write_dot_id(Source, G, Stream),
    write(Stream, ' -> '),
    write_dot_id(Target, G, Stream),

    write(Stream, ' [ style = '),
    kind2style(Kind, Style),
    write(Stream, Style),

    write(Stream, ', color = '),
    status2Color(Status, Color),
    write(Stream, Color),

    write(Stream, ', penwidth = '),
    status2thickness(Status, T),
    write(Stream, T),
    
    write(Stream, ', arrowhead = '),
    kind2headStyle(Kind, HeadStyle),
    write(Stream, HeadStyle),
    write(Stream, ' ];\n').

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
