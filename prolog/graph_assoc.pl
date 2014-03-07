
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

set_id_of_node(Id, (_,Identity, Edges, Constraint), (Id, Identity, Edges, Constraint)).
    
sig_string(G, tuple(ParamsIds), Str):-
    !,maplist(call(type_name_to_id(G)), ParamsNames0, ParamsIds), 
    maplist(full_name_to_local_name, ParamsNames0, ParamsNames),
    atomic_list_concat(ParamsNames,'_', Str). 
    
sig_string(G, ParamId, Str):- 
    type_name_to_id(G, Str0, ParamId), 
    full_name_to_local_name(Str0, Str).

namesig0(G, Name, arrow(ParamsIds,_), Kind, NameSig):-
    !,(Kind=method, Sep='__'; Kind=constructor, Sep='#_'),
    sig_string(G, ParamsIds, Ps), atomic_list_concat([Name, Sep, Ps], NameSig).

namesig0(_,Name, _, _, Name).

namesig(G, NodeId, NameSig):-
    get_node(NodeId, G, (_,(Kind, Name, Type),_,_)), 
    namesig0(G, Name, Type, Kind, NameSig).

namesig_of_node(Name, (_,(_,Name,_),_,_)):-!.

name_of_node(Name, (_,(_,Name,_),_,_)).

kind_of_node(Kind, (_,(Kind, _, _), _, _)).

type_of_node(Type, (_,(_, _, Type), _, _)).

identity_of_node(Identity, (_, Identity, _, _)).

container_of_node(C, (_, _, (C, _, _, _, _),_)).

containees_of_node(Cs, (_, _, (_, Cs, _, _, _) ,_)).

users_of_node(Users, (_,_,(_,_,Users, _, _),_)).

super_types_of_node(Super, (_,_,(_,_,_, Super, _),_)).

sub_types_of_node(Sub, (_,_,(_,_,_, _, Sub),_)).

%%%%%

    
create_node0(Type, Name, Sig, (none, (Type, Name, Sig), (no_parent, [],[], [], []), no_constraint)).

put_new_node(N, (Ns, Abs, Id), NId, (Ns1, Abs, Nb)):- 
    id_of_node(none, N), 
    set_id_of_node(Id, N, NId),
    put_assoc(Id, Ns, NId, Ns1), Nb is Id + 1.

create_node(Type, Name, Sig, GraphIn, NewNode, GraphOut):-
    create_node0(Type, Name, Sig, N),
    put_new_node(N, GraphIn, NewNode, GraphOut).

set_container((Id, Desc, (_, Cee, Users, Sup, Sub), Ct), Cer, (Id, Desc, (Cer, Cee, Users, Sup, Sub), Ct)).

add_containee((Id, Desc, (Cer, Cees, Users, Sup, Sub), Ct), Cee, (Id, Desc, (Cer, [Cee| Cees], Users, Sup, Sub), Ct)).

select_containee(Cee, (Id, Desc, (Cer, Cees, Users, Sup, Sub), Ct), (Id, Desc, (Cer, NCees, Users, Sup, Sub), Ct)):-
    select(Cee, Cees, NCees).

add_user((Id, Desc, (Cer, Cees,Users, Sup, Sub), Ct), User, (Id, Desc, (Cer, Cees, [User |Users], Sup, Sub), Ct)).
select_user(User, (Id, Desc, (Cer, Cees, Users, Sup, Sub), Ct), (Id, Desc, (Cer, Cees, NUsers, Sup, Sub), Ct)):-
    select(User, Users, NUsers).

add_super((Id, Desc, (Cer, Cees,Users, Sup, Sub), Ct), Super, (Id, Desc, (Cer, Cees, Users, [Super|Sup], Sub), Ct)).
select_super(Super, (Id, Desc, (Cer, Cees, Users, Sup, Sub), Ct), (Id, Desc, (Cer, Cees, Users, NSup, Sub), Ct)):-
    select(Super, Sup, NSup).

add_sub((Id, Desc, (Cer, Cees,Users, Sup, Subs), Ct), Sub, (Id, Desc, (Cer, Cees, Users, Sup, [Sub| Subs]), Ct)).
select_sub(Sub, (Id, Desc, (Cer, Cees, Users, Sup, Subs), Ct), (Id, Desc, (Cer, Cees, Users, Sup, NSubs), Ct)):-
    select(Sub, Subs, NSubs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions to build the graph on the fly while reading the file
build_graph(edge(contains, CerId, CeeId), (Ns, Abs, Nb), (Ns2, Abs, Nb)):-
    (get_assoc(CerId, Ns, Cer) -> add_containee(Cer, CeeId, NCer);
     NCer = (CerId, _, (no_parent, [CeeId], [], [], []), no_constraint)),
    put_assoc(CerId, Ns, NCer, Ns1),
    (get_assoc(CeeId, Ns1, Cee) -> set_container(Cee, CerId, NCee);
     NCee=(CeeId, _, (CerId,[], [], [], []), no_constraint)),
    put_assoc(CeeId, Ns1, NCee, Ns2).

build_graph(edge(uses, UserId, UseeId), (Ns, Abs, Nb), (Ns1, Abs, Nb)):-
    (get_assoc(UseeId, Ns, Usee) *->  add_user(Usee, UserId, NUsee);
     NUsee = (UseeId, _, (no_parent, [], [UserId], [], []), no_constraint)),
    put_assoc(UseeId, Ns, NUsee, Ns1).

build_graph(node(Id,Typ,Nm,Sig), (Ns, Abs, Nb), (Ns1, Abs, Nb1)):-
    Nb1 is Nb + 1,
    (get_assoc(Id, Ns, (Id, _, Edges, no_constraint)),!; Edges=(no_parent,[],[],[],[]),!),
    put_assoc(Id,  Ns, (Id,(Typ,Nm,Sig), Edges,no_constraint), Ns1).

build_graph(edge(isa, SubId, SuperId), (Ns, Abs, Nb), (Ns2, Abs, Nb)):-
    (get_assoc(SubId, Ns, Sub) -> add_super(Sub, SuperId, NSub);
     NSub = (SubId, _, (no_parent, [], [], [SuperId], []), no_constraint)),
    put_assoc(SubId, Ns, NSub, Ns1),
    (get_assoc(SuperId, Ns1, Super) -> add_sub(Super, SubId, NSuper);
     NSuper=(SuperId, _, (no_parent,[], [], [], [SubId]), no_constraint)),
    put_assoc(SuperId, Ns1, NSuper, Ns2).

read_graph_aux(Stream, GraphIn, GraphOut):-
    read_term(Stream, T, []),
    (T=end_of_file *-> GraphOut=GraphIn;
     build_graph(T, GraphIn, Graph2), read_graph_aux(Stream, Graph2, GraphOut)).

empty_graph((Ns0, Abs0, 0)):-
    empty_assoc(Ns0),
    empty_assoc(Abs0).

graph_read(FileName, (Ns1, Abs0, Nb0)):-
    open(FileName, read, Stream),
    empty_graph(G0),
    read_graph_aux(Stream, G0, (Ns0, Abs0, Nb0)),
    map_assoc(call(node_named_type_to_id((Ns0, Abs0, Nb0))), Ns0, Ns1),
    close(Stream),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function to build a graph from the prolog database
find_graph(G2):-
    empty_graph(G0),
    findall(node(Id, T, N, S), node(Id, T, N, S), Ns),
    foldl(build_graph, Ns, G0, G1), 
    findall(edge(K,O,T), edge(K,O,T), Es), %Kind Origin Target
    foldl(build_graph, Es, G1, G2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_node(Id, (Ns, _, _), N):- get_assoc(Id, Ns, N).

gen_node(Id, (Ns, _, _), N):- gen_assoc(Id, Ns, N).

put_node(N, (Ns, Abs, Nb), (Ns1, Abs, Nb)):- 
    id_of_node(Id, N), 
    put_assoc(Id, Ns, N, Ns1).

gen_syntaxicaly_correct_container(NodeK, Graph, Container):-
    gen_node(_, Graph, Container), 
        kind_of_node(ContainerK, Container),
        can_contain_kind(ContainerK, NodeK).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
uses(UserId, UseeId, G):-
    (ground(UseeId) -> get_node(UseeId, G, Usee);
     gen_node(UseeId, G, Usee)),
    users_of_node(UsersId, Usee),
    member(UserId, UsersId).

put_uses(UserId, UseeId, G, GOut):-
    get_node(UseeId, G, Usee), 
    add_user(Usee, UserId, NUsee),
    put_node(NUsee, G, GOut).

select_uses(UserId, UseeId, G, NewG):-
    (ground(UseeId) -> get_node(UseeId, G, Usee);
     gen_node(UseeId, G, Usee)),
    select_user(UserId, Usee, NUsee),
    put_node(NUsee, G, NewG).

redirect_uses(UserId, OldUseeId, NewUseeId, G, NewG):-
    select_uses(UserId, OldUseeId, G, G1),
    put_uses(UserId, NewUseeId, G1, NewG).

ids_to_use(SourceId, TargetId, (uses, SourceId, TargetId)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

contains(CerId, CeeId, G):- 
    ground(CeeId),
    get_node(CeeId, G, Cee), container_of_node(CerId, Cee).

contains(CerId, CeeId, G):- 
    ground(CerId),
    get_node(CerId, G, Cer), containees_of_node(CeesId, Cer),
    member(CeeId, CeesId).

contains(CerId, CeeId, G):- 
    \+ground(CerId), \+ground(CeeId),
    container_of_node(CerId, Cee),
    gen_node(CeeId, G, Cee).

is_root(N):- container_of_node(no_parent, N).

root(Id, G):- ground(Id), is_root(N), get_node(Id, G, N).
root(Id, G):- \+ground(Id), is_root(N), gen_node(Id, G, N).

leaf(Id, G):- ground(Id), containees_of_node([], N), get_node(Id, G, N).
leaf(Id, G):- \+ground(Id), containees_of_node([], N), gen_node(Id, G, N).


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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isa(SubId, SupId, G):-
    ground(SubId) -> get_node(SubId, G, Sub), super_types_of_node(SuperTypes, Sub), member(SupId, SuperTypes);
    (ground(SupId) -> get_node(SupId, G, Sup), sub_types_of_node(SubTypes, Sup), member(SubId, SubTypes);
     gen_node(SubId, G, Sub), super_types_of_node(SuperTypes, Sub), member(SupId, SuperTypes)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_abstractions(RealId, (_, Abs, _), AbsIds):-
    get_assoc(RealId, Abs, AbsIds).

gen_abstraction(RealId, (_,Abs,_), AbsId):-
    gen_assoc(RealId, Abs, AbsIds), member(AbsId, AbsIds).

add_abstraction(RealId, AbsId, (Ns, AbsAssoc, Nb), (Ns, NewAbsAssoc, Nb)):-
    get_assoc(RealId, AbsAssoc, AbsIds) -> put_assoc(RealId, AbsAssoc, [AbsId|AbsIds], NewAbsAssoc);
    put_assoc(RealId, AbsAssoc, [AbsId], NewAbsAssoc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
abstract_node(Node, G, Abs, GraphOut):-
    id_of_node(NodeId, Node), identity_of_node((Kind, Name, Type), Node),
    abstract_kind(Kind, AbsKind), 
    atomic_concat('abstract_', Name, AbsName), 
    create_node(AbsKind, AbsName, Type, G, Abs, G1),
    id_of_node(AbsId, Abs),
    add_abstraction(NodeId, AbsId, G1, G2),
    %% little hack (?) : an abstraction is its own abstraction otherwise,
    %% on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ... 
    %% and that can go on...
    add_abstraction(AbsId, AbsId, G2, GraphOut).

create_host(Node, GraphIn, Host, GraphOut):-
    identity_of_node((Kind, Name, _), Node),
    can_contain_kind(HostK, Kind),
    atomic_concat(Name, '_container', HostName),
    create_node(HostK, HostName, '', GraphIn, Host, GraphOut).

copy_contains_tree_aux(CerId, OriginalNodeId, (_,GraphIn), (CopyId, GraphOut)):-
    get_node(OriginalNodeId, GraphIn, OriginalNode),
    identity_of_node((Kind, Name, Type), OriginalNode),
    create_node(Kind, Name, Type, GraphIn, Copy, G0), id_of_node(CopyId, Copy),
    %if copy an abstraction, register it as an abstraction
    (gen_abstraction(RealId, G0, OriginalNodeId) *-> 
        add_abstraction(RealId, CopyId, G0, G1); G1=G0),

    (CerId=no_parent*-> G2=G1;
        put_contains(CerId, CopyId, G1, G2)),
    containees_of_node(Cees, OriginalNode),
    foldl(call(copy_contains_tree_aux(CopyId)), Cees, (_,G2), (_,GraphOut)).

copy_contains_tree(NodeId, GraphIn, NodeCopyId, GraphOut):-
    copy_contains_tree_aux(no_parent, NodeId, (_,GraphIn), (NodeCopyId, GraphOut)).
