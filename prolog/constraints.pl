:-module(constraints,
	 [read_constraints/3,
	  is_violation/3,
	  collect_constraints/3,
	  %collect_constraints/4,
	  interloper/3,
	  empty_constraint/1]).
:-use_module(graph).

fold_cts_fn_to_id(Graph, Imports, CtFN, List, [CtID | List]):-
    constraint_full_name_to_id(Graph, Imports, CtFN, CtID).

read_constraint0((SetDefs, Imports, Cts), declareSet(Name,Values), (NewSetDefs, Imports, Cts)):-
    put_assoc(Name, SetDefs, Values, NewSetDefs),!.

read_constraint0((SetDefs, Imports, Cts), java_import(Values), (SetDefs, NewImports, Cts)):-
    append(Values, Imports, NewImports),!.

read_constraint0((SetDefs, Imports, Cts), Ct, (SetDefs, Imports, [Ct|Cts])).

read_constraints_aux(Stream, CtsIn, CtsOut):-
	read_term(Stream, T, []), 
	(T=end_of_file *-> CtsIn=CtsOut;
	 read_constraint0(CtsIn, T , CtsOut0),
	 read_constraints_aux(Stream, CtsOut0, CtsOut)).

normalize_constraints_aux(Graph, Imports, SetDefs, RootFN, Ct, CtsAcc, CtsAcc2):-
    findall(NCt, normal_constraint(NCt, RootFN, SetDefs, Ct),NCts0),
    foldl(call(fold_cts_fn_to_id(Graph, Imports)), NCts0, CtsAcc, CtsAcc2).

normalize_constraints(Graph, Imports, SetDefs, RootFN, Cts, NCts):-
    foldl(call(normalize_constraints_aux(Graph, Imports, SetDefs, RootFN)), Cts, [], NCts).

decorate_graph_aux(Id-Ct, Ns, NewNs):-
    get_assoc(Id, Ns, (Id, Desc, Edges, no_constraint)),
    put_assoc(Id, Ns, (Id, Desc, Edges, Ct), NewNs).

decorate_graph(CtsAssoc, (Ns, AbsAssoc, Nb), (NewNs, AbsAssoc, Nb)):-
    assoc_to_list(CtsAssoc, CtsList),
    foldl(decorate_graph_aux, CtsList, Ns, NewNs).

read_constraints(FileName, GraphIn, GraphOut):-
    get_roots([RootID], GraphIn), full_name_to_id(GraphIn, RootFN, RootID),
    open(FileName, read, Stream),
    empty_assoc(SetDefs0),
    read_constraints_aux(Stream, (SetDefs0, [], []), (SetDefs, Imports, CtList)),
    normalize_constraints(GraphIn, Imports, SetDefs, RootFN, CtList, NCtList),
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

rev_append_aux(X, Ys, [X| Ys]).

rev_append(L1, L2, L3):- foldl(rev_appends_aux, L1, L2, L3).

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

full_name_to_id_with_imports(Graph, _, FN, ID):- full_name_to_id(Graph, FN, ID),!.
full_name_to_id_with_imports(Graph, Imports, LocalName, ID):- 
    member(Package, Imports), atomic_list_concat([Package, '.', LocalName], FN),
    full_name_to_id(Graph, FN, ID),!.

constraint_full_name_to_id(Graph, Imports, (SFN, InterlopersFN, FriendsFN), (SID, InterlopersID, FriendsID)):-
			   full_name_to_id_with_imports(Graph, Imports, SFN, SID),
			   maplist(call(full_name_to_id_with_imports(Graph, Imports)), InterlopersFN, InterlopersID),
			   maplist(call(full_name_to_id_with_imports(Graph, Imports)), FriendsFN, FriendsID).


%%%%%%%%%%%%%%%%

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

empty_constraint(([], [], [])).

interloper(_, Ct, _):-empty_constraint(Ct), !, false.
interloper(Id, (Is, IsWF, Fs), Graph):-
    interloper0(Id, Is, Fs, Graph);
    member((Is1, Fs1), IsWF), append(Fs1, Fs, Fs2), interloper0(Id, Is1, Fs2).



is_violation(UserId, UseeId, Graph):- 
    find_constraint_node(UseeId, Graph, HNode),
    id_of_node(HId, HNode), constraint_of_node((Is, IsWF, _), HNode), 
    collect_friends(HId, Graph, [], Fs), interloper(UserId, (Is, IsWF, Fs), Graph).
    

collect_constraints_aux(no_parent, _, Cts, Cts).
collect_constraints_aux(Id, G, (Is, IsWF, Fs), Cts):-
    get_node(Id, G, Node), constraint_of_node(Cts0, Node), container_of_node(CerId, Node),
    (Cts0=no_constraint *-> collect_constraints_aux(CerId, G, (Is, IsWF, Fs), Cts);
     Cts0=(Is0, IsWF0, Fs0), append(Is0, Is, Is1), append(IsWF0, IsWF, IsWF1), append(Fs0, Fs, Fs1),
     collect_constraints_aux(CerId, G, (Is1, IsWF1, Fs1), Cts)).

collect_constraints(Id, G, Cts):- collect_constraints_aux(Id, G, ([],[],[]), Cts).

interloper_of(UserId, UseeId, Graph):-
    collect_constraints(UseeId, Graph, Cts),
    interloper(UserId, Cts, Graph).

