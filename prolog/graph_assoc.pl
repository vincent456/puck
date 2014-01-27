
%% node :
%% (Id, Description, Edges, Constraint).
%% Description = (Type, Name, Sig)
%% Edges = (ContainedBy, Contains)
%% Constraint cf constraint.pl
%% graph:
%% (Nodes, Uses, Nb nodes)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getters to encapsulate node representation
%%

id_of_node(Id, (Id,_,_,_)).
    
name_of_node(NameSig, (_,(_,Name,Signature),_,_)):-
    atom_concat(Name,Signature,NameSig).

type_of_node(Type, (_,(Type, _, _), _, _)).

container_of_node(C, (_, _, (C, _),_)).

containees_of_node(Cs, (_, _, (_, Cs) ,_)).

%%%%%

create_node(Id, Type, Name, Sig, (Id, (Type, Name, Sig), (no_parent, []), no_constraint)).

set_container((Id, Desc, (_, Cee), Ct), Cer, (Id, Desc, (Cer, Cee), Ct)).
add_containee((Id, Desc, (Cer, Cees), Ct), Cee, (Id, Desc, (Cer, [Cee| Cees]), Ct)).

select_containee(Cee, (Id, Desc, (Cer, Cees), Ct), (Id, Desc, (Cer, NCees), Ct)):-
    select(Cee, Cees, NCees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions to build the graph on the fly while reading the file
build_graph(edge(contains, CerId, CeeId), (Ns,Us, Nb), (NewNs,Us, Nb)):-
    (get_assoc(CerId, Ns, Cer) *-> add_containee(Cer, CeeId, NCer);
     NCer = (CerId, _, (no_parent, [CeeId]), no_constraint)),
    put_assoc(CerId, Ns, NCer, Ns1),
    (get_assoc(CeeId, Ns1, Cee) *-> set_container(Cee, CerId, NCee);
     NCee=(CeeId, _, (CerId,[]), no_constraint)),
    put_assoc(CeeId, Ns1, NCee, NewNs).

build_graph(node(Id,Typ,Nm,Sig), (Ns,Us,Nb), (NewNs, Us, Nb1)):-
    Nb1 is Nb + 1,
    (get_assoc(Id, Ns, (Id, _, Edges, no_constraint)) *-> true; Edges=(no_parent,[])),
    put_assoc(Id,  Ns, (Id,(Typ,Nm,Sig), Edges,no_constraint), NewNs).

build_graph(edge(uses, UserId, UseeId), (Ns, Us, Nb), (Ns, [(uses, UserId, UseeId)|Us], Nb)).

build_graph(edge(isa, _, _), G, G).


read_graph_aux(Stream, GraphIn, GraphOut):-
    read_term(Stream, T, []),
    (T=end_of_file *-> GraphOut=GraphIn;
     build_graph(T, GraphIn, Graph2), read_graph_aux(Stream, Graph2, GraphOut)).

read_graph(FileName, Graph):-
    open(FileName, read, Stream),
    empty_assoc(Ns),
    read_graph_aux(Stream,(Ns, [], 0), Graph),
    close(Stream),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functions to build a graph from the prolog database
find_children(Id, CeesId):-
    findall(CId, edge(contains, Id, CId), CeesId).

find_node_family(Id, no_parent, CeesId):-
    \+edge(contains,_,Id), find_children(Id,CeesId).

find_node_family(Id, CerId, CeesId):-
    edge(contains, CerId, Id), find_children(Id, CeesId).

node_assoc(NsList, NsAssoc):- empty_assoc(Acc), node_assoc_aux(NsList, Acc, NsAssoc).
node_assoc_aux([], Acc, Acc).
node_assoc_aux([N | NsList], Acc, NsAssoc):-
    id_of_node(Id, N), put_assoc(Id, Acc, N, Acc2),
    node_assoc_aux(NsList, Acc2, NsAssoc).

find_graph((Ns,Us,Nb)):-
    findall(Id-(Id, (Typ, Nm, Sig), (CerId, CeesId), no_constraint), 
	    (node(Id,Typ,Nm,Sig), find_node_family(Id, CerId, CeesId)), NsList),
    length(NsList, Nb), list_to_assoc(NsList, Ns),
    findall((uses,USrc, UTgt), edge(uses,USrc, UTgt), Us).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_node(Id, N, (Ns, _, _)):- get_assoc(Id, Ns, N).

gen_node(Id, N, (Ns,_,_)):- gen_assoc(Id, Ns, N).

put_node(N, (Ns, U, Nb), (NewNs, U, Nb1)):- 
    id_of_node(Id, N), put_assoc(Id, Ns,  N, NewNs), Nb1 is Nb + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
uses(UserId, UseeId, (_, U, _)):-
    member((uses,UserId, UseeId), U).

put_uses(UserId, UseeId, (Ns, Us, Nb), (Ns, [(uses, UserId, UseeId) |Us], Nb)).

select_uses(UserId, UseeId, (N, U, Nb), (N, NewU, Nb)):-
	       select((uses, UserId, UseeId), U, NewU).

ids_to_use(SourceId, TargetId, (uses, SourceId, TargetId)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

contains(CerId, CeeId, (Ns, _, _)):- 
    ground(CeeId),
    get_assoc(CeeId, Ns, Cee), container_of_node(CerId, Cee).

contains(CerId, CeeId, (Ns, _, _)):- 
    ground(CerId),
    get_assoc(CerId, Ns, Cer), containees_of_node(CeesId, Cer),
    member(CeeId, CeesId).

contains(CerId, CeeId, G):- 
    \+ground(CerId), \+ground(CeeId),
    container_of_node(CerId, Cee),
    gen_node(CeeId, Cee, G).

is_root(N):- container_of_node(no_parent, N).

root(Id, G):- ground(Id), is_root(N), get_node(Id, N, G).
root(Id, G):- \+ground(Id), is_root(N), gen_node(Id, N, G).

leaf(Id, G):- ground(Id), containees_of_node([], N), get_node(Id, N, G).
leaf(Id, G):- \+ground(Id), containees_of_node([], N), gen_node(Id, N, G).


put_contains(CerId, CeeId, (Ns, Us, Nb), (Ns3, Us, Nb)):-
    get_assoc(CerId, Ns, Cer), add_containee(Cer, CeeId, NCer),
    put_assoc(CerId, Ns, NCer, Ns2),
    get_assoc(CeeId, Ns, Cee), set_container(Cee, CerId, NCee),
    put_assoc(CeeId, Ns2, NCee, Ns3).


select_contains(CerId, CeeId, (Ns, Us, Nb), (Ns3, Us, Nb)):-
    get_assoc(CerId, Ns, Cer), select_containee(CeeId, Cer, NCer),
    put_assoc(CerId, Ns, NCer, Ns2),
    get_assoc(CeeId, Ns, Cee), set_container(Cee, no_parent, NCee),
    put_assoc(CeeId, Ns2, NCee, Ns3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abstract_node((NodeId, (Type, Name, Sig), _, _), (Ns, Us, AbsId), AbsAssocs, 
	      Abs, NewG, NewAbsAssocs):-
    abstract_type(Type, AbsType), 
    atomic_concat('abstract_', Name, AbsName), 
    create_node(AbsId, AbsType, AbsName, Sig, Abs),
    put_assoc(NodeId, AbsAssocs, AbsId, NewAbsAssocs1),
    %% little hack (?) : an abstraction is its own abstraction otherwise,
    %% on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ... 
    %% and that can go on...
    put_assoc(AbsId, NewAbsAssocs1, AbsId, NewAbsAssocs), 
    put_node(Abs, (Ns, Us, AbsId), NewG).


