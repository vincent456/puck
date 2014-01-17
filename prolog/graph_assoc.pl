
%% node :
%% (Id, Type, Name, Sig, ContainedBy, Contains).
%%
%% graph:
%% (Nodes, Uses, Nb nodes)

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
    findall(Id-(Id,Typ, Nm, Sig, CerId, CeesId), 
	    (node(Id,Typ,Nm,Sig), find_node_family(Id, CerId, CeesId)), NsList),
    length(NsList, Nb), list_to_assoc(NsList, Ns),
    findall((uses,USrc, UTgt), edge(uses,USrc, UTgt), Us).

get_node(Id, N, (Ns, _, _)):- get_assoc(Id,Ns, N).

put_node(N, (Ns, U, Nb), (NewNs, U, Nb1)):- 
    id_of_node(Id, N), put_assoc(Id, Ns,  N, NewNs), Nb1 is Nb + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getters to encapsulate node representation
%%

id_of_node(Id, (Id,_,_,_,_,_)).
    
name_of_node(NameSig, (_,_,Name,Signature,_,_)):-
    atom_concat(Name,Signature,NameSig).

type_of_node(Type, (_,Type, _, _, _, _)).

container_of_node(C, (_, _, _, _, C, _)).

containees_of_node(Cs, (_, _, _, _, _, Cs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


contains(CerId, CeeId, (Ns, _, _)):- 
    \+ground(CerId), \+ground(CeeId),
    assoc_to_list(Ns, NsList),
    member(CeeId- Cee, NsList), container_of_node(CerId, Cee).

root(Id, (Ns, _, _)):-get_assoc(Id, Ns, N), container_of_node(no_parent, N).
leaf(Id, (Ns, _, _)):-get_assoc(Id, Ns, N), containees_of_node([], N).

put_contains(CerId, CeeId, (Ns, Us, Nb), (Ns3, Us, Nb)):-
    get_assoc(CerId, Ns, (CerId, CerT, CerN, CerS, CerCer, CerCees)),
    put_assoc(CerId, Ns, (CerId, CerT, CerN, CerS, CerCer, [CeeId | CerCees]), Ns2),
    get_assoc(CeeId, Ns, (CeeId, CeeT, CeeN, CeeS, _, CeeCees)),
    put_assoc(CeeId, Ns2, (CeeId, CeeT, CeeN, CeeS, CerId, CeeCees), Ns3).


select_contains(CerId, CeeId, (Ns, Us, Nb), (Ns3, Us, Nb)):-
    get_assoc(CerId, Ns, (CerId, CerT, CerN, CerS, CerCer, CerCees)),
    select(CeeId, CerCees, NewCerCees),
    put_assoc(CerId, Ns, (CerId, CerT, CerN, CerS, CerCer, NewCerCees), Ns2),
    get_assoc(CeeId, Ns, (CeeId, CeeT, CeeN, CeeS, CerId, CeeCees)),
    put_assoc(CeeId, Ns2, (CeeId, CeeT, CeeN, CeeS, no_parent, CeeCees), Ns3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abstract_node((NodeId, Type, Name, Sig, _, _), (Ns, Us, AbsId), AbsAssocs, 
	      Abs, NewG, NewAbsAssocs):-
    abstract_type(Type, AbsType), 
    atomic_concat('abstract_', Name, AbsName),
    Abs=(AbsId, AbsType, AbsName, Sig, no_parent, []),
    put_assoc(NodeId, AbsAssocs, AbsId, NewAbsAssocs1),
    %% little hack (?) : an abstraction is its own abstraction otherwise,
    %% on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ... 
    %% and that can go on...
    put_assoc(AbsId, NewAbsAssocs1, AbsId, NewAbsAssocs), 
    put_node(Abs, (Ns, Us, AbsId), NewG).


