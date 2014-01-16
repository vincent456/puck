:-module(constraintsTranformations,
	 [constraintsListFullName2Ids/3]).
:-use_module(graph).

constraintsListFullName2Ids(FNcts, IdsCts, Graph):-
    constraintsListFullName2Ids(FNcts, [], IdsCts, Graph).

constraintsListFullName2Ids([], IdsCts, IdsCts, _).

constraintsListFullName2Ids([hideFrom(HideeFN,InterloperFN)| FNcts] , Acc, IdsCts, Graph):-
    full_name_from_id(Hid, HideeFN, Graph), full_name_from_id(Iid, InterloperFN, Graph),
    constraintsListFullName2Ids(FNcts, [hideFrom(Hid, Iid)| Acc], IdsCts, Graph).

constraintsListFullName2Ids([isFriendOf(HideeFN,InterloperFN)|FNcts] , Acc, IdsCts, Graph):-
    full_name_from_id(Hid, HideeFN, Graph), full_name_from_id(Iid, InterloperFN, Graph),
    constraintsListFullName2Ids(FNcts, [isFriendOf(Hid, Iid)| Acc], IdsCts, Graph).






