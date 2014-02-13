:-module(constraints,
	 [read_constraints/3,
	  is_violation/3,
	  collect_constraints/3,
	  collect_constraints/4,
	  interloper/3,
	  empty_constraint/1]).
:-use_module(graph).

%%% named set

% gMember(A,B):- member(A,B).
% gMember(A,B):- declaredMember(A,B).

% gSelect(A,B,C) :- select(A,B,C).
% gSelect(A,B,C) :- selectedMember(A,B,C).

% declaredMember(Element, SetName):- declareSet(SetName, Set),
%				    member(Element, Set).

% declaredMember(Element, UnionName) :- declareSetUnion(UnionName, SetList),
%				      member(Set, SetList), gMember(Element, Set).

% declaredMember(Element, SetName):- collectSubtypesOf(SuperType, SetName),
%	'isa+'(Element, SuperType).

% selectedMember(Element, ListName, OtherElements) :-
%	bagof(E,declaredMember(E, ListName), List), select(Element, List, OtherElements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold_cts_fn_to_id(Graph, CtFN, List, [CtID | List]):-
    constraint_full_name_to_id(CtFN, CtID, Graph).

read_constraint0(SetDefs, Cts, declareSet(Name,Values), NewSetDefs, Cts):-
    put_assoc(Name, SetDefs, Values, NewSetDefs),!.
read_constraint0(SetDefs, Cts, Ct, SetDefs, [Ct|Cts]).

read_constraints_aux(Stream, SetsDefsAcc, CtsAcc, SetsDefs, Cts):-
	read_term(Stream, T, []), 
	(T=end_of_file *-> Cts=CtsAcc, SetsDefs=SetsDefsAcc;
	 read_constraint0(SetsDefsAcc, CtsAcc, T , SetsDefsAcc2, CtsAcc2),
	 read_constraints_aux(Stream, SetsDefsAcc2, CtsAcc2, SetsDefs, Cts)).

normalize_constraints_aux(Graph, SetDefs, RootFN, Ct, CtsAcc, CtsAcc2):-
    findall(NCt, normal_constraint(NCt, RootFN, SetDefs, Ct),NCts0),
    foldl(call(fold_cts_fn_to_id(Graph)), NCts0, CtsAcc, CtsAcc2).

normalize_constraints(Graph, SetDefs, RootFN, Cts, NCts):-
    foldl(call(normalize_constraints_aux(Graph, SetDefs, RootFN)), Cts, [], NCts).

%% read_constraints_aux(Stream, RootFN, Graph, CtAcc, Cts):-
%% 	read_term(Stream, T, []), 
%% 	(T=end_of_file *-> Cts=CtAcc;
%% 	 findall(NCt, normal_constraint(NCt, RootFN, T),NCts),
%% 	 foldl(call(fold_cts_fn_to_id(Graph)), NCts, CtAcc, CtAcc2),
%% 	 read_constraints_aux(Stream, RootFN, Graph, CtAcc2, Cts)).


%% it's not satisfactory to translate rootId to its fullname to make the translation the other way
%% in read_constraints_aux, but I don't find any more satisfying solution
%% either the translation would have to be done in normal_constraint, (and then, it has to be written numerous time)
%% either a test can be add in full_name_to_id
%% anyway the root has a short name so the translation is not too costly

decorate_graph_aux(Id-Ct, Ns, NewNs):-
    get_assoc(Id, Ns, (Id, Desc, Edges, no_constraint)),
    put_assoc(Id, Ns, (Id, Desc, Edges, Ct), NewNs).

decorate_graph(CtsAssoc, (Ns, AbsAssoc, Nb), (NewNs, AbsAssoc, Nb)):-
    assoc_to_list(CtsAssoc, CtsList),
    foldl(decorate_graph_aux, CtsList, Ns, NewNs).

read_constraints(FileName, GraphIn, GraphOut):-
    get_roots([RootID], GraphIn), full_name_to_id(RootFN, RootID, GraphIn),
    open(FileName, read, Stream),
    empty_assoc(SetDefs0),
    read_constraints_aux(Stream, SetDefs0, [], SetDefs, CtList),
    normalize_constraints(GraphIn, SetDefs, RootFN, CtList, NCtList),
    close(Stream),
    group_constraints(NCtList, CtsAssoc),
    decorate_graph(CtsAssoc, GraphIn, GraphOut),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% normal_constraint(-NC, +RootFullName, +UserConstraint)
% NC : ( hidee : Scope * interlopers : Scope Set * friends : Scope Set)
%%

%% !!!! find a way to factorize all these call to get_set_def !!!!
get_set_def(Set, _, Set):- is_list(Set),!.
get_set_def(SetName, Env, SetDef):-get_assoc(SetName, Env, SetDef).

normal_constraint((S, InterlopersDef, []), _, SetDefs, hideScopeFrom(S, Interlopers)):- 
    get_set_def(Interlopers, SetDefs, InterlopersDef),!.

normal_constraint((S, InterlopersDef, []), _, SetDefs, hideScopeSetFrom(ScopeSet, Interlopers)):-
    get_set_def(Interlopers, SetDefs, InterlopersDef),
    get_set_def(ScopeSet, SetDefs, ScopeSetDef), !,
    member(S, ScopeSetDef).

normal_constraint((Member, OtherActors,[]), _, SetDefs,hideFromEachOther(Actors)):-
    get_set_def(Actors, SetDefs, ActorsDef),!,
    select(Member, ActorsDef, OtherActors).

normal_constraint((S, [Root], FriendsDef), Root, SetDefs, hideScopeButFrom(S, Friends)):-
    get_set_def(Friends, SetDefs, FriendsDef),!.

normal_constraint((S, [Root], FriendsDef), Root, SetDefs, hideScopeSetButFrom(ScopeSet, Friends)):-
    get_set_def(Friends, SetDefs, FriendsDef),
    get_set_def(ScopeSet, SetDefs, ScopeSetDef), !,
    member(S, ScopeSetDef).

normal_constraint((S, [Root], []), Root, _, hideScope(S)).
normal_constraint((S, [Root], []), Root, SetDefs, hideScopeSet(ScopeSet)):-
    get_set_def(ScopeSet, SetDefs, ScopeSetDef), !,
    member(S,ScopeSetDef).

normal_constraint((S, InterlopersDef, FriendsDef), _, SetDefs, hideScope(S, Interlopers, Friends)):-
    get_set_def(Interlopers, SetDefs, InterlopersDef),
    get_set_def(Friends, SetDefs, FriendsDef),!.

normal_constraint((S, InterlopersDef, FriendsDef), _, SetDefs, hideScopeSet(ScopeSet, Interlopers, Friends)):-
    get_set_def(Interlopers, SetDefs, InterlopersDef),
    get_set_def(Friends, SetDefs, FriendsDef),
    get_set_def(ScopeSet, SetDefs, ScopeSetDef), !,
    member(S,ScopeSetDef).


normal_constraint((Befriended, [], [User]), _, _, isFriendOfScope(User, Befriended)).
normal_constraint((Befriended, [], [User]), _, SetDefs,isFriendOfScopeSet(User, BefriendedSet)):-
    get_set_def(BefriendedSet, SetDefs, BefriendedSetDef),!,
    member(Befriended, BefriendedSetDef).

normal_constraint((Befriended, [], UsersDef), _, SetDefs, areFriendOfScope(Users, Befriended)):-
    get_set_def(Users, SetDefs, UsersDef).

normal_constraint((Befriended, [], UsersDef), _, SetDefs, areFriendOfScopeSet(Users, BefriendedSet)):-
    get_set_def(Users, SetDefs, UsersDef),
    get_set_def(BefriendedSet, SetDefs, BefriendedSetDef),!,
    member(Befriended, BefriendedSetDef).

%%%
%%(hidee, interloper list, (interloper * friend) list, friend, %% hideeAncestorConstraint, hideeDescendantConstraint)
%%%%%%%%%%%%%%%

non_empty_list([]):- !, false.
non_empty_list(L):- is_list(L).

%% empty constraint, do nothing
add_constraint((_, [], []), CtsAssoc, CtsAssoc):-!.

add_constraint((HideeID, Interlopers, []), CtsAssoc, NewCtsAssoc):-
    get_assoc(HideeID, CtsAssoc, (OldInterlopers, IsWFs, Friends))
	     *-> append(Interlopers, OldInterlopers, Is), %% make it a set ??
    put_assoc(HideeID, CtsAssoc, (Is, IsWFs, Friends), NewCtsAssoc);
    put_assoc(HideeID, CtsAssoc, (Interlopers,[], []), NewCtsAssoc).

add_constraint((BefriendedID, [], Friends), CtsAssoc, NewCtsAssoc):-
    get_assoc(BefriendedID, CtsAssoc, (Is, IsWF, OldFs))
	     *-> append(Friends, OldFs, Fs),
    put_assoc(BefriendedID, CtsAssoc, (Is, IsWF, Fs), NewCtsAssoc);
    put_assoc(BefriendedID, CtsAssoc, ([], [], Friends), NewCtsAssoc).

add_constraint((ScopeID, Interlopers,Friends), CtsAssoc, NewCtsAssoc):-
    non_empty_list(Interlopers), non_empty_list(Friends),
    (get_assoc(ScopeID, CtsAssoc, (Is, IsWF, Fs))
	     *-> put_assoc(ScopeID, CtsAssoc, (Is, [(Interlopers, Friends)| IsWF] , Fs), NewCtsAssoc);
    put_assoc(ScopeID, CtsAssoc, ([], [(Interlopers, Friends)] , []), NewCtsAssoc)).

group_constraints(CtList, Assoc) :- 
    empty_assoc(A), foldl(add_constraint, CtList, A, Assoc).

%%%%%%%%%%%

list_full_name_to_ids_aux(Graph, FName, IdList, [Id | IdList]):-
    full_name_to_id(FName, Id, Graph).

list_full_name_to_ids(FNList, IDList, Graph) :-
    foldl(call(list_full_name_to_ids_aux(Graph)), FNList, [], IDList).

constraint_full_name_to_id((SFN, InterlopersFN, FriendsFN), (SID, InterlopersID, FriendsID), Graph):-
			   full_name_to_id(SFN, SID, Graph),
			   list_full_name_to_ids(InterlopersFN, InterlopersID, Graph),
			   list_full_name_to_ids(FriendsFN, FriendsID, Graph).


%%%%%%%%%%%%%%%%

constraint_of_node(Ct, (_,_,_, Ct)).

find_constraint_node(no_parent, _, _):- !, fail.

find_constraint_node(Id, G, Hn):-
    get_node(Id, G, N), 
    (constraint_of_node(no_constraint, N) *->
		       container_of_node(Cer, N), find_constraint_node(Cer, G, Hn);
     Hn= N).

%%

collect_friends(no_parent, _, Fs, Fs).

collect_friends(Id, G, Fs1, Fs):-
    get_node(Id, G, N), constraint_of_node(no_constraint, N),
    container_of_node(Cer, N), collect_friends(Cer, G, Fs1, Fs).

collect_friends(Id, G, Fs1, Fs):-
    get_node(Id, G, N), constraint_of_node((_,_, Fs2), N), append(Fs2, Fs1, Fs3),
    container_of_node(Cer, N), collect_friends(Cer, G, Fs3, Fs).

%%

scope_set_contained(ScopeId, ScopeIdList, Graph):-
    member(Id, ScopeIdList), 'contains*'(Id, ScopeId, Graph).

interloper0(UserId, InterloperIdList, FriendIdList, Graph):-
    scope_set_contained(UserId, InterloperIdList, Graph),
    \+ scope_set_contained(UserId, FriendIdList, Graph).

interloper(Id, (Is, IsWF, Fs), Graph):-
    interloper0(Id, Is, Fs, Graph);
    member((Is1, Fs1), IsWF), append(Fs1, Fs, Fs2), interloper0(Id, Is1, Fs2).



is_violation(UserId, UseeId, Graph):- 
    find_constraint_node(UseeId, Graph, HNode),
    id_of_node(HId, HNode), constraint_of_node((Is, IsWF, _), HNode), 
    collect_friends(HId, Graph, [], Fs), interloper(UserId, (Is, IsWF, Fs), Graph).
    

collect_constraints(no_parent, _, Cts, Cts).
collect_constraints(Id, G, (Is, IsWF, Fs), Cts):-
    get_node(Id, G, Node), constraint_of_node(Cts0, Node), container_of_node(CerId, Node),
    (Cts0=no_constraint *-> collect_constraints(CerId, G, (Is, IsWF, Fs), Cts);
     Cts0=(Is0, IsWF0, Fs0), append(Is0, Is, Is1), append(IsWF0, IsWF, IsWF1), append(Fs0, Fs, Fs1),
     collect_constraints(CerId, G, (Is1, IsWF1, Fs1), Cts)).

collect_constraints(Id, G, Cts):- collect_constraints(Id, G, ([],[],[]), Cts).

empty_constraint(([],[],[])).
