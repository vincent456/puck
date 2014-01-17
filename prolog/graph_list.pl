
find_graph(graph(Ns,Cs,Us)):-
    findall(node(Id,Typ,Nm, Sig), node(Id,Typ,Nm,Sig), Ns),
    findall(edge(contains,CSrc, CTgt), edge(contains,CSrc, CTgt), Cs),
    findall(edge(uses,USrc, UTgt), edge(uses,USrc, UTgt), Us).

get_node(NodeId, node(NodeId, Ntype, Nname, Nsig), graph(Nodes,_,_)):-
    member(node(NodeId, Ntype, Nname, Nsig), Nodes).

put_node(Node, graph(N,C,U), graph([Node|N], C,U)).

select_node(NodeId, node(NodeId, Ntype, Nname, Nsig), graph(N,C,U), graph(NewN,C,U)):-
    select(node(NodeId, Ntype, Nname, Nsig), N, NewN).

id_of_node(NodeId, node(NodeId,_,_,_)).
    
name_of_node(NameSig, node(_,_,Name,Signature)):-
    atom_concat(Name,Signature,NameSig).

type_of_node(Type, node(_,Type, _,_)).

uses(UserId, UseeId, graph(_,_,U)):-
    member(edge(uses,UserId, UseeId), U).

put_uses(UserId, UseeId, graph(N,C,U), graph(N,C, [edge(uses, UserId, UseeId)|U])).

select_uses(UserId, UseeId, graph(N,C,U), graph(N,C,NewU)):-
	       select(edge(uses,UserId, UseeId), U, NewU).

contains(ContainerId, ContaineeId, graph(_,C,_)):-
    member(edge(contains,ContainerId, ContaineeId), C).

root(Id,G) :- \+contains(_,Id,G).
leaf(Id,G) :- \+contains(Id,_,G).

put_contains(ContainerId, ContaineeId, graph(N,C,U), graph(N,[edge(contains, ContainerId, ContaineeId)|C], U)).

select_contains(ContainerId, ContaineeId, graph(N,C,U), graph(N,NewC,U)):-
    select(edge(contains,ContainerId, ContaineeId), C, NewC).

ids_to_use(SourceId, TargetId, edge(uses,SourceId, TargetId)).

abstract_node(node(NodeId, Type, Name, Sig), graph(N,C,U), AbsAssocs, 
	      Abs, graph([ Abs | N ], C, U), NewAbsAssocs):-
    abstract_type(Type, AbsType), 
    atomic_concat('abstract_', Name, AbsName),
    length(N, AbsId),
    Abs=node(AbsId, AbsType, AbsName, Sig),
    put_assoc(NodeId, AbsAssocs, AbsId, NewAbsAssocs1),
    %% little hack (?) : an abstraction is its own abstraction otherwise,
    %% on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ... 
    %% and that can go on...
    put_assoc(AbsId, NewAbsAssocs1, AbsId, NewAbsAssocs).
