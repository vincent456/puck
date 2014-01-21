:-module(constraintsTranformations,
	 [constraintsListFullName2Ids/3]).
:-use_module(graph).

%%% named set

gMember(A,B):- member(A,B).
gMember(A,B):- declaredMember(A,B).

gSelect(A,B,C) :- select(A,B,C).
gSelect(A,B,C) :- selectedMember(A,B,C).

declaredMember(Element, SetName):- declareSet(SetName, Set),
				    member(Element, Set).

declaredMember(Element, UnionName) :- declareSetUnion(UnionName, SetList),
				      member(Set, SetList), gMember(Element, Set).

%% declaredMember(Element, SetName):- collectSubtypesOf(SuperType, SetName),
%% 	'isa+'(Element, SuperType).

selectedMember(Element, ListName, OtherElements) :- 
	bagof(E,declaredMember(E, ListName), List), select(Element, List, OtherElements).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Root = param en entré
% hide_ : first param is a scope, other params are scopes set
% hide_set: all param are scope set
% hide_scope(_set) : hidee(s), interlopers, friends
% friendsOf
%%

% lightweight constraint   
hide(S, Interlopers, [], _)            :- hide_scope_from(S, Interlopers).
%hideScopeSet(ScopeSet, [], Interlopers, [])   :- hideScopeSetFrom(ScopeSet, Interlopers).

hide(Member, OtherActors,[], _) :- hide_from_each_other(Actors), gSelect(Member, Actors, OtherActors).

hide(S, Root, Friends, Root)       :- hide_scope_but_from(S, Friends).

%% hideScopeButFrom(S, AllFriends)    :- hideScopeSetButFrom(ScopeSet, Friends), 
%% 				      friends(ScopeSet, Friends, AllFriends), gMember(S, ScopeSet).

hide(S, Root, [], Root)               :- hide_scope(S).
%% hideScopeButFrom(S, Friends)        :- hideScopeSet(ScopeSet),
%% 				       friends(ScopeSet, Friends), gMember(S,ScopeSet).


%%%
%%(hidee, interloper list, (interloper * friend) list, friend, %% hideeAncestorConstraint, hideeDescendantConstraint)
%%%%%%%%%%%%%%%
add_hide((Hidee, NewInterlopers, []), CtsAssoc, NewCtsAssoc):-
    get_assoc(Hidee, CtsAssoc, (Hidee, Interlopers, InterlopersWithFriends, Friends)),
    append(NewInterlopers, Interlopers, Is), %% make it a set ??
    put_assoc(Hidee, CtsAssoc, (Hidee, Is, InterlopersWithFriends, Friends), NewCtsAssoc).

add_hide((Hidee, Interlopers, Friends), CtsAssoc, NewCtsAssoc):-
    get_assoc(Hidee, CtsAssoc, (Hidee, Interlopers, InterlopersWithFriends, Friends)),
    put_assoc(Hidee, CtsAssoc, (Hidee, Interlopers, [(Interlopers, Friends)| InterlopersWithFriends] , Friends), NewCtsAssoc).

add_hide((Hidee, Interlopers, []), CtsAssoc, NewCtsAssoc):-
    \+get_assoc(Hidee, CtsAssoc,_), 
    put_assoc(Hidee, CtsAssoc, (Hidee, Interlopers,[], []), NewCtsAssoc).

add_hide((Hidee, Interlopers, Friends), CtsAssoc, NewCtsAssoc):-
    \+get_assoc(Hidee, CtsAssoc,_), 
    put_assoc(Hidee, CtsAssoc, (Hidee, [],[(Interlopers,Friends)], []), NewCtsAssoc).

%% if it is not hidden from anybody, we can ignore the friend rule
add_friend((_,Befriended), CtsAssoc, CtsAssoc):- \+get_assoc(Befriended, CtsAssoc,_).

add_friend((Users,Befriended), CtsAssoc, NewCtsAssoc):- 
    get_assoc(Befriended, CtsAssoc, (Befriended, Is, IsWF, Friends)),
    append(Users, Friends, Fs), 
    put_assoc(Befriended, CtsAssoc, (Befriended, Is, IsWF, Fs), NewCtsAssoc).


build_constraints(HideList, FriendList, Assoc) :-
    empty_assoc(A), foldl(add_hide, HideList, A, A2),
    foldl(add_friend, FriendList, A2, Assoc).
%%%%%%

find_constraints(Constraints, Graph):-
    findall(hideFrom(Hidee,Interloper), hideFrom(Hidee,Interloper), FNCts0),
    findall(isFriendOf(User,Usee), isFriendOf(User,Usee), FNCts, FNCts0),
    constraintsListFullName2Ids(FNCts, Constraints, Graph).

constraintsListFullName2Ids(FNcts, IdsCts, Graph):-
    constraintsListFullName2Ids(FNcts, [], IdsCts, Graph).

constraintsListFullName2Ids([], IdsCts, IdsCts, _).

constraintsListFullName2Ids([hideFrom(HideeFN,InterloperFN)| FNcts] , Acc, IdsCts, Graph):-
    full_name_from_id(Hid, HideeFN, Graph), full_name_from_id(Iid, InterloperFN, Graph),
    constraintsListFullName2Ids(FNcts, [hideFrom(Hid, Iid)| Acc], IdsCts, Graph).

constraintsListFullName2Ids([isFriendOf(HideeFN,InterloperFN)|FNcts] , Acc, IdsCts, Graph):-
    full_name_from_id(Hid, HideeFN, Graph), full_name_from_id(Iid, InterloperFN, Graph),
    constraintsListFullName2Ids(FNcts, [isFriendOf(Hid, Iid)| Acc], IdsCts, Graph).



