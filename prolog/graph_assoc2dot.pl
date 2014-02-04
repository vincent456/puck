:-module(pl2dot, 
	 [pl2dot/2,
	  pl2dot/3]).
:-use_module(graph).

pl2dot(File, Graph) :- pl2dot(File, Graph, []).

pl2dot(File, G, V):- 
    open(File, write, Stream, []),
    write(Stream, 'digraph G {\n'), %linux dotty parser doesn't accept anonym graph %'
    write(Stream, 'rankdir=LR; ranksep=2;\n'),
    graph2dot(G, Stream),
    violations2dot(V, G, Stream),
    write(Stream, '}\n'),
    close(Stream).

graph2dot(G, Stream):- 
    get_roots(Ids, G),
    foldl(writeln_node, Ids, (G, Stream), (G, Stream)).

writeln_contains(Cee, (Cer, G, S), (Cer, G, S)):- writeln_edge((contains, Cer, Cee), (G,S), (G,S)).
writeln_uses(User, (Usee, G, S), (Usee, G, S)):- writeln_edge((uses, User, Usee), (G,S), (G,S)).

write_label(Id, (Nb, G, S), (Nb1, G, S)):-
    Nb1 is Nb +1,
    write(S, ' | <f'), write(S, Nb),write(S, '> '), 
    get_node(Id, (Id, (Kind,Name,_),_,_), G), write(S, Name), 
    ((Kind=method; Kind=constructor) *-> write(S, '()'); true),
    write(S, ' ('),write(S, Id),write(S, ')').

writeln_hooked_node(Id, (G,S), (G,S)):-
    get_node(Id, (Id, _, (_, _, Users), _), G),
    foldl(writeln_uses, Users, (Id, G, S), _).

writeln_node(Id, (G, S), (G, S)):-
    get_node(Id, N, G),
    type_of_node(Kind, N),
    name_of_node(Name,N),
    containees_of_node(Cees, N),
    users_of_node(Users, N),
    (subgraph(Kind) *-> writeln_subgraph((Id,Kind,Name,Cees, Users), (G,S));
     (hook(Kind)*-> writeln_hook((Id,Kind,Name,Cees, Users), (G,S)); false)).
	    
writeln_subgraph((Id,Kind,Name,Cees, Users) , (G,S)):-
    write(S, 'subgraph cluster'), write(S, Id), write(S, ' {\n'),
    write(S, 'label= "\\<\\<'), write(S, Kind), write(S, '\\>\\> '), write(S, Name), write(S,' ";\n'),
    write(S, 'color=black;\n'),	
    foldl(writeln_node, Cees, (G, S), _), 
    write(S,'}\n'),
    foldl(writeln_uses, Users, (Id, G, S), _).


writeln_hook( (Id,Kind,Name,Cees, Users), (G, S)) :- 
    write(S, Id),
    write(S, ' [ label = "<f0> \\<\\<'),
    write(S, Kind),
    write(S,'\\>\\> '),
    write(S, Name), write(S, ' ('),write(S, Id),write(S, ')'),
    (hook(Kind)*-> foldl(write_label, Cees, (1, G, S), _); true),
    write(S,'"\n'),
    write(S, ', shape = "record", style = filled, fillcolor = '),
    node_kind_to_fill_color(Kind, Color),
    write(S, Color),
    write(S, ' ];\n'),
    (hook(Kind) *-> foldl(writeln_hooked_node, Cees, ( G, S), _);
     foldl(writeln_node, Cees, (G, S), _), 
     foldl(writeln_contains, Cees, (Id, G, S), _)),
    foldl(writeln_uses, Users, (Id, G, S), _).

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
