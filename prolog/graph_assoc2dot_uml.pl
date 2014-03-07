:-module(graph_assoc2dot, 
	 [pl2dot/2,
	  pl2dot/3]).
:-use_module(graph).

pl2dot(File, Graph) :- pl2dot(File, Graph, []).

pl2dot(File, G, V):- 
    open(File, write, Stream, []),
    write(Stream, 'digraph G {\n'), %linux dotty parser doesn't accept anonym graph %'
    write(Stream, 'rankdir=LR; ranksep=2; compound=true\n'),
    graph2dot(G, Stream),
    violations2dot(V, G, Stream),
    write(Stream, '}\n'),
    close(Stream).

graph2dot(G, Stream):- 
    get_roots(Ids, G),
    maplist(call(writeln_node(Stream, G)), Ids).

writeln_contains(S, G, Cer, Cee):- writeln_edge(S, G, correct, (contains, Cer, Cee)).
writeln_uses(S, G, Usee, User):- writeln_edge(S, G, correct, (uses, User, Usee)).
writeln_isa(S, G, Sub, Sup):- writeln_edge(S, G, correct, (isa, Sub, Sup)).

writeln_hooked_node(S, G, Id):-
    get_node(Id, G, N), users_of_node(Users,N),
    maplist(call(writeln_uses(S, G, Id)), Users).

extract_node(N, (Id,Kind,Name,Cees, Users, Supers)):-
    id_of_node(Id, N),
    kind_of_node(Kind, N),
    name_of_node(Name,N),
    containees_of_node(Cees, N),
    users_of_node(Users, N),
    super_types_of_node(Supers, N).

writeln_node(S, G, Id):-
    get_node(Id, G, N),
    kind_of_node(Kind, N),
    (subgraph(Kind) *-> writeln_subgraph(S, G, N);
     hook(Kind), writeln_hook(S, G, N)).
	    
writeln_subgraph(S, G, N):-
    extract_node(N, (Id,Kind,Name,Cees, Users, _)),
    atomic_list_concat(['subgraph cluster', Id,' {\n', 
			'label= "\\<\\<', Kind, '\\>\\> ', Name, ' (',Id,') ";\n', 'color=black;\n'], Str),
    write(S, Str),	
    (length(Cees,0)*-> write(S, Id), write(S, ' [label="" shape=none ]');
     maplist(call(writeln_node(S,G)), Cees)),
    write(S,'}\n'),
    maplist(call(writeln_uses(S, G, Id)), Users).

sort_class_aux(method, N, (Attrs, Cts, Ms, Others), (Attrs, Cts, [N|Ms], Others)):-!.
sort_class_aux(constructor, N, (Attrs, Cts, Ms, Others), (Attrs, [N|Cts], Ms, Others)):-!.
sort_class_aux(attribute, N, (Attrs, Cts, Ms, Others), ([N|Attrs], Cts, Ms, Others)):-!.
sort_class_aux(_, N, (Attrs, Cts, Ms, Others), (Attrs, Cts, Ms, [N|Others])).

sort_class(G, Id, SortIn, SortOut):-
    get_node(Id,G,N), kind_of_node(K, N), sort_class_aux(K, N, SortIn, SortOut).

write_tr(S, N) :-
    id_of_node(Id, N),
    name_of_node(Name, N),
    atomic_list_concat(['<TR><TD PORT="', Id,'" ALIGN="LEFT" BORDER="0">', Name,' (',Id,')</TD></TR>\n'], Str),
    write(S, Str).

writeln_hook(S, G, N) :- 
    extract_node(N, (Id,Kind,Name,Cees, Users, Supers)),
    node_kind_to_fill_color(Kind, Color),
    (Kind=interface*-> atomic_list_concat(['&lt;&lt;interface&gt;&gt;<BR/>', Name, ' (', Id, ')'],UmlName);
     atomic_list_concat([Name, ' (', Id,')'], UmlName)),
    atomic_list_concat([Id, ' [ label = <<TABLE BGCOLOR="', Color,'"> <TR> <TD PORT="', Id, '" BORDER="0"> <B>', UmlName, ' </B></TD></TR>\n'], NodeStr),
    write(S, NodeStr), 
    foldl(call(sort_class(G)), Cees, ([],[],[],[]), (Attrs, Cts, Ms, Others)),
    length(Attrs, LA),
    length(Ms, LM0),
    length(Cts, LC),
    LM is LC + LM0,
    ((LA\=0 ; LM\=0) -> write(S, '<HR/>'); true),
    maplist(call(write_tr(S)), Attrs),
    ((LA\=0, LM\=0) -> write(S, '<HR/>'); true),
    maplist(call(write_tr(S)), Cts),
    maplist(call(write_tr(S)), Ms),
    write(S,'</TABLE>>, shape = "none" ];\n'),
    maplist(call(writeln_hook(S,G)), Others),
    maplist(call(writeln_hooked_node(S,G)), Cees),
    maplist(call(writeln_uses(S, G, Id)), Users),
    maplist(call(writeln_isa(S, G, Id)), Supers).


violations2dot(Vs, G, S):- maplist(call(writeln_edge(S,G,incorrect)), Vs).

dot_id(G, Id, DotId):-
    get_node(Id, G, N), kind_of_node(T, N), 
    ((hook(T) *-> CerId=Id; container_of_node(CerId, N)),
     atomic_list_concat([CerId,':',Id], DotId),!; 
     subgraph(T), DotId=Id).

writeln_edge(Stream, G, Status, (Kind, Source, Target)) :- 
    dot_id(G, Source, SId),
    dot_id(G, Target, TId),
    kind2style(Kind, Style),
    status2Color(Status, Color),
    status2thickness(Status, Thick),
    kind2headStyle(Kind, HeadStyle),
    subgraph_arc(G, Source, 'ltail', TailStr),
    subgraph_arc(G, Target, 'lhead', HeadStr),
    atomic_list_concat(
	    [SId, ' -> ', TId, ' [', TailStr, HeadStr, 'style = ', Style, 
	     ', color = ', Color, ', penwidth = ', Thick, ', arrowhead = ', HeadStyle, ' ];\n'], Str),
    write(Stream, Str).

subgraph_arc(G, Id, Position, Str):- 
    get_node(Id, G, N), kind_of_node(K, N), 
    (subgraph(K)*-> atomic_list_concat([Position, '=cluster',Id,', '], Str); %because of the comma do not put Str as last attribute
     Str='').

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
