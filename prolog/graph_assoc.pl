
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

id_of_node(Id, (Id,_,_,_,_)).

set_id_of_node(Id, (_,Identity, Edges, UsesDepencies, Constraint), 
                    (Id, Identity, Edges, UsesDepencies, Constraint)).

identity_of_node(Identity, (_, Identity, _, _, _)).

set_identity_of_node(Identity, (Id, _, Edges, UsesDepencies, Constraint),
                                (Id, Identity, Edges, UsesDepencies, Constraint)).

edges_of_node(Edges, (_, _, Edges, _, _)).

set_edges_of_node(NEdges, (Id, Identity, _, UsesDepencies, Constraint),
                           (Id, Identity, NEdges, UsesDepencies, Constraint)).

uses_dependencies_of_node(UD, (_, _, _, UD, _)).

set_uses_dependencies_of_node(NUD, (Id, Identity, Edges, _, Constraint), 
                                    (Id, Identity, Edges, NUD, Constraint)).

constraint_of_node(Ct, (_, _, _, _, Ct)).
set_constraint_of_node(Ct, (Id, Identity, Edges, UD, _),
                            (Id, Identity, Edges, UD, Ct)).

%%%%%%%%%%%%%%%%%%%%%

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
    get_node(NodeId, G, N),
    identity_of_node((Kind, Name, Type), N), 
    namesig0(G, Name, Type, Kind, NameSig).
%%%%%%%%%%%%%%%%%%%%%%%%%

name_of_node(Name, N):-
    identity_of_node((_, Name, _), N).    

kind_of_node(Kind, N):-
    identity_of_node((Kind, _, _), N).    

type_of_node(Type, N):- 
    identity_of_node((_, _, Type), N).

%%%
container_of_node(C, N):-
    edges_of_node((C, _, _, _, _), N).

containees_of_node(Cs, N):-
    edges_of_node((_, Cs, _, _, _), N).

users_of_node(Users, N):-
    edges_of_node((_,_,Users, _, _), N).

super_types_of_node(Super, N):-
    edges_of_node((_, _, _, Super, _), N).

sub_types_of_node(Sub, N):-
    edges_of_node((_, _, _, _, Sub), N).

side_uses_of_node(SUses, N):-
    uses_dependencies_of_node((SUses, _), N).

primary_uses_of_node(PUses, N):-
    uses_dependencies_of_node((_, PUses), N).

    
%%%%%

create_node0(Desc, 
    (none, Desc, 
        (no_parent, [], [], [], []), 
        (A1, A2),
        no_constraint)):-
    empty_assoc(A1),
    empty_assoc(A2).

empty_node(N):-create_node0(_, N).

put_new_node(N, (Ns, Abs, Id), NId, (Ns1, Abs, Nb)):- 
    id_of_node(none, N), 
    set_id_of_node(Id, N, NId),
    put_assoc(Id, Ns, NId, Ns1), Nb is Id + 1.

create_node(Type, Name, Sig, GraphIn, NewNode, GraphOut):-
    create_node0((Type, Name, Sig), N),
    put_new_node(N, GraphIn, NewNode, GraphOut).

set_container(N, Cer, NN):-
    edges_of_node((_, Cee, Users, Sup, Sub), N),
    set_edges_of_node((Cer, Cee, Users, Sup, Sub), N, NN).

add_containee(N, Cee, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Sub), N),
    set_edges_of_node((Cer, [Cee| Cees], Users, Sup, Sub), N, NN).

select_containee(Cee, N, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Sub), N),
    select(Cee, Cees, NCees),
    set_edges_of_node((Cer, NCees, Users, Sup, Sub), N, NN).

add_user(N, User, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Sub), N),
    set_edges_of_node((Cer, Cees, [User |Users], Sup, Sub), N, NN).

    

select_user(User, N, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Sub), N),
    select(User, Users, NUsers),
    set_edges_of_node((Cer, Cees, NUsers, Sup, Sub), N, NN).

add_super(N, Super, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Sub), N),
    set_edges_of_node((Cer, Cees, Users, [Super|Sup], Sub), N, NN).

select_super(Super, N, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Sub), N),
    select(Super, Sup, NSup),
    set_edges_of_node((Cer, Cees, Users, NSup, Sub), N, NN).

add_sub(N, Sub, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Subs), N),
    set_edges_of_node((Cer, Cees, Users, Sup, [Sub| Subs]), N, NN).

select_sub(Sub, N, NN):-
    edges_of_node((Cer, Cees, Users, Sup, Subs), N),
    select(Sub, Subs, NSubs),
    set_edges_of_node((Cer, Cees, Users, Sup, NSubs), N, NN).


add_without_duplicate(E, Set, Set):- member(E, Set),!.
add_without_duplicate(E, Set, [E|Set]).

add_side_uses(N, PrimaryUserId, SideUse, NN):-
    uses_dependencies_of_node((PUAssoc, SUAssoc), N),
    (get_assoc(PrimaryUserId, SUAssoc, SUses),!; SUses=[],!),
    add_without_duplicate(SideUse, SUses, NSUses),
    put_assoc(PrimaryUserId, SUAssoc, NSUses, NSUAssoc),
    set_uses_dependencies_of_node((PUAssoc, NSUAssoc), N, NN).

set_side_uses(N, PrimaryUserId, NSUses, NN):-
    uses_dependencies_of_node((PUAssoc, SUAssoc), N),
    put_assoc(PrimaryUserId, SUAssoc, NSUses, NSUAssoc),
    set_uses_dependencies_of_node((PUAssoc, NSUAssoc), N, NN).

del_side_uses(N, PrimaryUserId, NN):-
    uses_dependencies_of_node((PUAssoc, SUAssoc), N),
    del_assoc(PrimaryUserId, SUAssoc, _, NSUAssoc),
    set_uses_dependencies_of_node((PUAssoc, NSUAssoc), N, NN).

get_side_uses((PrimaryUserId, PrimaryUseeId), G, SUses):-
    get_node(PrimaryUseeId, G, PrimaryUsee),
    side_uses_of_node(SUsesAssoc, PrimaryUsee),
     (get_assoc(PrimaryUserId, SUsesAssoc, SUses),!; SUses=[],!).


add_primary_uses(N, SideUserId, PrimaryUse, NN):-
    uses_dependencies_of_node((PUAssoc, SUAssoc), N),
    (get_assoc(SideUserId, PUAssoc, PUses),!; PUses=[],!),
    add_without_duplicate(PrimaryUse, PUses, NPUses),
    put_assoc(SideUserId, PUAssoc, NPUses, NPUAssoc),
    set_uses_dependencies_of_node((NPUAssoc, SUAssoc), N, NN).

set_primary_uses(N, SideUserId, NPUses, NN):-
    uses_dependencies_of_node((PUAssoc, SUAssoc), N),
    put_assoc(SideUserId, PUAssoc, NPUses, NPUAssoc),
    set_uses_dependencies_of_node((NPUAssoc, SUAssoc), N, NN).

del_primary_uses(N, SideUserId, NN):-
    uses_dependencies_of_node((PUAssoc, SUAssoc), N),
    del_assoc(SideUserId, PUAssoc, _, NPUAssoc),
    set_uses_dependencies_of_node((NPUAssoc, SUAssoc), N, NN).

get_primary_uses((SideUserId, SideUseeId), G, PUses):-
    get_node(SideUseeId, G, SideUsee),
    primary_uses_of_node(PUsesAssoc, SideUsee),
     (get_assoc(SideUserId, PUsesAssoc, PUses),!; PUses=[],!).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% functions to build the graph on the fly while reading the file
get_node_or_create_empty(Id, G, Node):- get_node(Id, G, Node), !.
get_node_or_create_empty(Id, _, Node):- empty_node(N0), 
    set_id_of_node(Id, N0, Node),!.

build_graph(edge(contains, CerId, CeeId), G, G2):-
    get_node_or_create_empty(CerId, G, Cer),
    add_containee(Cer, CeeId, NCer),
    put_node(NCer, G, G1),
    
    get_node_or_create_empty(CeeId, G1, Cee),
    set_container(Cee, CerId, NCee),
    put_node(NCee, G1, G2).

build_graph(edge(uses, UserId, UseeId), G, G1):-
    get_node_or_create_empty(UseeId, G, Usee),
    add_user(Usee, UserId, NUsee),
    put_node(NUsee, G, G1).

build_graph(node(Id,Typ,Nm,Sig), (Ns, Abs, Nb), (Ns1, Abs, Nb1)):-
    Nb1 is Nb + 1,
    get_node_or_create_empty(Id, (Ns, Abs, Nb), Node),
    set_identity_of_node((Typ,Nm,Sig), Node, NNode),
    put_assoc(Id, Ns, NNode, Ns1).

build_graph(edge(isa, SubId, SuperId), G, G2):-
    get_node_or_create_empty(SubId, G, Sub),
    add_super(Sub, SuperId, NSub), 
    put_node(NSub, G, G1),

    get_node_or_create_empty(SuperId, G1, Super),
    add_sub(Super, SubId, NSuper),
    put_node(NSuper, G1, G2).

%% 
%% uses_dependency( method call uses, variable declaration type uses)
build_graph(uses_dependency((SideUserId, SideUseeId), 
                                (PrimaryUserId, PrimaryUseeId)), 
                                    G, G3):-
    get_node_or_create_empty(PrimaryUseeId, G, PrimaryUsee),
    add_side_uses(PrimaryUsee, PrimaryUserId, (SideUserId, SideUseeId), NPrimaryUsee),
    put_node(NPrimaryUsee, G, G2),
    get_node_or_create_empty(SideUseeId, G2, SideUsee),
    add_primary_uses(SideUsee, SideUserId, (PrimaryUserId, PrimaryUseeId), NSideUsee),
    put_node(NSideUsee, G2, G3).



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
