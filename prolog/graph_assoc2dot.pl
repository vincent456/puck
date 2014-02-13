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
    maplist(call(writeln_node(Stream, G)), Ids).

%% order of arguments matters !!, predicate used with maplist
writeln_contains(S, G, Cer, Cee):- writeln_edge(S, G, correct, (contains, Cer, Cee)).
writeln_uses(S, G, Usee, User):- writeln_edge(S, G, correct, (uses, User, Usee)).
writeln_isa(S, G, Sub, Sup):- writeln_edge(S, G, correct, (isa, Sub, Sup)).

%write_label(Id, (Nb, G, S), (Nb1, G, S)):-
write_label(S, G, Id, Nb, Nb1):-
    Nb1 is Nb +1,
    write(S, ' | <f'), write(S, Nb),write(S, '> '), 
    get_node(Id, G, (Id, (Kind,Name,_),_,_)), write(S, Name), 
    ((Kind=method; Kind=constructor) *-> write(S, '()'); true),
    write(S, ' ('),write(S, Id),write(S, ')').

writeln_hooked_node(S, G, Id):-
    get_node(Id, G, N), users_of_node(Users,N),
    maplist(call(writeln_uses(S, G, Id)), Users).

writeln_node(S, G, Id):-
    get_node(Id, G, N),
    kind_of_node(Kind, N),
    name_of_node(Name,N),
    containees_of_node(Cees, N),
    users_of_node(Users, N),
    (subgraph(Kind) *-> writeln_subgraph(S, G, (Id,Kind,Name,Cees, Users));
     hook(Kind), super_types_of_node(Supers, N),
     writeln_hook(S,G, (Id,Kind,Name,Cees, Users, Supers))).
	    
writeln_subgraph(S, G, (Id,Kind,Name,Cees, Users)):-
    write(S, 'subgraph cluster'), write(S, Id), write(S, ' {\n'),
    write(S, 'label= "\\<\\<'), write(S, Kind), write(S, '\\>\\> '), write(S, Name), write(S,' ";\n'),
    write(S, 'color=black;\n'),	
    maplist(call(writeln_node(S,G)), Cees),
    write(S,'}\n'),
    maplist(call(writeln_uses(S, G, Id)), Users).

writeln_hook(S, G, (Id,Kind,Name,Cees, Users, Supers)) :- 
    write(S, Id),
    write(S, ' [ label = "<f0> \\<\\<'),
    write(S, Kind),
    write(S,'\\>\\> '),
    write(S, Name), write(S, ' ('),write(S, Id),write(S, ')'),
    foldl(call(write_label(S,G)), Cees, 1, _),
    write(S,'"\n'),
    write(S, ', shape = "record", style = filled, fillcolor = '),
    node_kind_to_fill_color(Kind, Color),
    write(S, Color),
    write(S, ' ];\n'),
    maplist(call(writeln_hooked_node(S,G)), Cees),
    maplist(call(writeln_uses(S, G, Id)), Users),
    maplist(call(writeln_isa(S, G, Id)), Supers).


violations2dot(Vs, G, S):- maplist(call(writeln_edge(S,G,incorrect)), Vs).

write_dot_id(S, G, Id):-
    get_node(Id, G, N), kind_of_node(T, N), 
    (record_top(T) *-> write_dot_id_aux(S, Id, 0);
     container_of_node(CerId, N),
     get_node(CerId, G, Cer), containees_of_node(Cees,Cer),
     nth1(Nth, Cees, Id), write_dot_id_aux(S, CerId, Nth)).

write_dot_id_aux(Stream, Src, Nth):-write(Stream, Src), write(Stream, ':f'), write(Stream, Nth).

writeln_edge(Stream, G, Status, (Kind, Source, Target)) :- 
    write_dot_id(Stream, G, Source),
    write(Stream, ' -> '),
    write_dot_id(Stream, G, Target),

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


kind2style(isa, dashed).
kind2style(contains, dashed).
kind2style(virtualContains, dashed).
kind2style(uses, bold).

kind2headStyle(isa, empty).
kind2headStyle(contains, 'open').
kind2headStyle(virtualContains, 'open').
kind2headStyle(uses, normal).
