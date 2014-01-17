:-module(graph, 
	 [   %graph getters & query
	     find_graph/1,
	     get_node/3,
	     contains/3,
	     'contains*'/3,
	     uses/3,
	     
	     name_from_id/3,
	     full_name_from_id/3,
	     
	     %graph setters
	     put_node/3,
	     put_uses/4,
	     put_contains/4,
	     
	     abstract_node/6, %exposed for javaRules and sigmaRules, the solver should use abstract/6

	     % "extracter" behave as lists:select but with elements of the graph
	     select_node/4,
	     select_uses/4,
	     select_contains/4,
	     	     
	     %node getters
	     name_of_node/2,
	     type_of_node/2,
	     id_of_node/2,
	     ids_to_use/3,
	     can_contain/2]).

:-use_module(javaRules).
:-reexport(javaRules, [abstract/6]).
%% :-use_module(sigmaRules).
%% :-reexport(sigmaRules, [abstract/6]).

find_graph(graph(Ns,Cs,Us)):-
    findall(node(Id,Typ,Nm, Sig), node(Id,Typ,Nm,Sig), Ns),
    findall(edge(contains,CSrc, CTgt), edge(contains,CSrc, CTgt), Cs),
    findall(edge(uses,USrc, UTgt), edge(uses,USrc, UTgt), Us).


%% getters and setters to encapsulate Graph structure
get_node(NodeId, node(NodeId, Ntype, Nname, Nsig), graph(Nodes,_,_)):-
    member(node(NodeId, Ntype, Nname, Nsig), Nodes).

put_node(Node, graph(N,C,U), graph([Node|N], C,U)).

select_node(NodeId, node(NodeId, Ntype, Nname, Nsig), graph(N,C,U), graph(NewN,C,U)):-
    select(node(NodeId, Ntype, Nname, Nsig), N, NewN).

id_of_node(NodeId, node(NodeId,_,_,_)).
    
name_of_node(NameSig, node(_,_,Name,Signature)):-
    atom_concat(Name,Signature,NameSig).

type_of_node(Type, node(_,Type, _,_)).

can_contain(node(_,HostType,_,_), node(_,Type,_,_)):- can_contain_type(HostType,Type).

name_from_id(NodeId,NameSig, Graph):- 
    get_node(NodeId, Node, Graph), name_of_node(NameSig, Node).

uses(UserId, UseeId, graph(_,_,U)):-
    member(edge(uses,UserId, UseeId), U).

put_uses(UserId, UseeId, graph(N,C,U), graph(N,C, [edge(uses, UserId, UseeId)|U])).

select_uses(UserId, UseeId, graph(N,C,U), graph(N,C,NewU)):-
	       select(edge(uses,UserId, UseeId), U, NewU).

contains(ContainerId, ContaineeId, graph(_,C,_)):-
    member(edge(contains,ContainerId, ContaineeId), C).

put_contains(ContainerId, ContaineeId, graph(N,C,U), graph(N,[edge(contains, ContainerId, ContaineeId)|C], U)).

select_contains(ContainerId, ContaineeId, graph(N,C,U), graph(N,NewC,U)):-
    select(edge(contains,ContainerId, ContaineeId), C, NewC).

'contains*'(X,X, Graph) :- get_node(X, _, Graph).
'contains*'(X,Z, Graph) :- contains(X,Y,Graph), 'contains*'(Y,Z, Graph).

ids_to_use(SourceId, TargetId, edge(uses,SourceId, TargetId)).

%% source_id_of_arc(Source, edge(_,Source,_)).
%% target_id_of_arc(Target, edge(_,_,Target)).

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

abstract(node(NodeId,Type, Name, Sig), graph(N,C,U), Abs, Use,
	 graph([Abs | N], C, [Use | U]) ):-
    %% genId don't know the cost of length, threading an id generator is another option
    abstract_type(Type, AbsType), length(N, AbsId),
    abstract_uses(AbsType, NodeId, AbsId, UserId, UseeId),
    atomic_concat('abstract_', Name, AbsName),
    node(AbsId,AbsType, AbsName,Sig) = Abs,
    edge(uses, UserId, UseeId) = Use.


%%%%%%%%%%%%%%%%

full_name_from_id(NodeId,FullName, Graph):- 
		path_to_node(NodeId,Path, Graph),
		names_on_path(Path, Names, Graph),
		atomic_list_concat(Names,'.',FullName).

%removeDefaultPackage(Names,NamesWithoutDefaultPackage),
%% atomic_list_concat(NamesWithoutDefaultPackage,'.',FullName).

%% removeDefaultPackage([NameOfDefaultPackage|Names], Names):- 
%% 		name_of_node(dp,NameOfDefaultPackage), !.
%% removeDefaultPackage(Names,Names).

%%%%%%%%%%%%

names_on_path(NodesIds, Names, Graph):- 
    names_on_path(NodesIds, [], RevNames, Graph), reverse(RevNames, Names).

names_on_path([],Acc,Acc, _).
names_on_path([NodeId|NodesIds],Acc, Names,Graph):- 
    name_from_id(NodeId, Name, Graph),
    names_on_path(NodesIds, [Name | Acc], Names, Graph).

% toplevel : no owner					   
toplevel(NodeId, Graph):- \+contains(_,NodeId, Graph).					   

% Path to Node through scopes (Ids only)
path_to_node(Node,Path, Graph):- path(Node,[],Path, Graph).
		
path(Top,Path,[Top|Path], Graph):-
		toplevel(Top, Graph).%,!.
path(NodeId,Path,Result, Graph):-
		contains(OwnerId, NodeId, Graph),
		path(OwnerId,[NodeId|Path], Result, Graph).
