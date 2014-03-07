
:-module(solver,
	 [solve/2,
	  find_violations/3,
	  solver_read/3,
	  solver_read_and_solve/2,
	  solver_read_and_print_violations/1]).

:-use_module(library(assoc)).
:-use_module(graph).
:-reexport(graph).
:-use_module(constraints).
:-reexport(constraints).

max_number_host(3).

solver_read(GraphFile, ConstraintFile, Graph):-
    graph_read(GraphFile, G), read_constraints(ConstraintFile, G, Graph),!.

solver_read_and_solve(BaseName, G3):-
    atomic_list_concat([BaseName, '.pl'], GFile),
    atomic_list_concat([BaseName, '_decouple.pl'], DecoupleFile),
    atomic_list_concat([BaseName, '_before.dot'], BeforeFile),
    atomic_list_concat([BaseName, '_after.dot'], AfterFile),
    solver_read(GFile, DecoupleFile, G),
    find_violations(G, Vs, G2), pl2dot(BeforeFile, G2, Vs),
    solve(G, G3), pl2dot(AfterFile, G3).

solver_read_and_print_violations(BaseName):-
    atomic_list_concat([BaseName, '.pl'], GFile),
    atomic_list_concat([BaseName, '_decouple.pl'], DecoupleFile),
    atomic_list_concat([BaseName, '.dot'], DotFile),
    solver_read(GFile, DecoupleFile, G),
    find_violations(G, Vs, G2), pl2dot(DotFile, G2, Vs).

excluded(Id, List, Value):- get_assoc(Id,List,Value).
excluded(Id, List, []):- \+get_assoc(Id,List,_).

%%%
% we search a host for an abstraction who use its realisation
%% ex : delegate call
user_host(Real, CeeK, WrongUsersId, Graph, HostId):-

    id_of_node(RealId, Real),
    collect_constraints(RealId, Graph, RCts),!,

    gen_syntaxicaly_correct_container(CeeK, Graph, Host),
    id_of_node(HostId, Host),

    \+interloper(HostId, RCts, Graph),

    collect_constraints(HostId, Graph, HCts),
    (empty_constraint(HCts);
    	forall(member(UserId, WrongUsersId),
				\+interloper(UserId, HCts, Graph))).

%%%
% we search a host an abstraction used by its realisation
% ex: abstraction = interface, realisation = class
usee_host(Real, CeeK, WrongUsersId, Graph, HostId):-

    id_of_node(RealId, Real),

    gen_syntaxicaly_correct_container(CeeK, Graph, Host),
    id_of_node(HostId, Host),

    collect_constraints(HostId, Graph, HCts),
    (empty_constraint(HCts);
	  	\+interloper(RealId, HCts, Graph),
     forall(member(UserId, WrongUsersId),
		\+interloper(UserId, HCts, Graph))).

find_host(Real, CeeK, WrongUsersId, GraphIn, HostId):-
    real_kind_use_abs_kind(Real) -> usee_host(Real, CeeK, WrongUsersId, GraphIn, HostId);
     user_host(Real, CeeK, WrongUsersId, GraphIn, HostId).

%%%%%

permited_users_usee_abs(RealId, ViolationsId, Graph, PotentialHostId,
		(PotentialHostId, PermitedUsers)):-
	collect_constraints(PotentialHostId, Graph, Cts),
	findall(User, (member(User, ViolationsId),
					\+interloper(User, Cts, Graph),
					\+interloper(RealId, Cts, Graph)),
		PermitedUsers).


permited_users_usee_real(RealId, ViolationsId, Graph, PotentialHostId,
		(PotentialHostId, PermitedUsers)):-
	collect_constraints(RealId, Graph, CtsReal),
	collect_constraints(PotentialHostId, Graph, Cts),
	findall(User, (member(User, ViolationsId),
					\+interloper(User, Cts, Graph),
					\+interloper(PotentialHostId, CtsReal, Graph)),
		PermitedUsers).


%% compute a list of pair (HostId, "subset of violations id solved with this host")
potential_host_set(Real, ViolationsId, Graph, PHSet):-
    kind_of_node(RealK, Real), abstract_kind(RealK, AbsK),
	findall(H, gen_syntaxicaly_correct_container(AbsK, Graph, H), Hosts),
		id_of_node(RealId, Real),
        (real_kind_use_abs_kind(Real) ->
	maplist(call(permited_users_usee_abs(RealId, ViolationsId, Graph)), Hosts, PHSet);
	maplist(call(permited_users_usee_real(RealId, ViolationsId, Graph)), Hosts, PHSet)).

%% filter_aux(Pred, Elt, L, LOut):-
%% 	call(Pred, Elt)-> LOut = [Elt| L]; LOut=L.

%% filter(Pred, ListIn, ListOut):-
%% 	foldl(call(filter_aux(Pred)), ListIn, [], ListOut).


member_for_call(List, Elt):-member(Elt, List).

%%% find host for abstractions where the original wrong user can use it without
%%% violating any contstraint
%% à réimplenter en utilisant une structure union-find ?
attrib_host_aux1(_,[], B, B).
attrib_host_aux1(PotentialHostsId, ViolationsIdIn, BindingsIn, BindingsOut):-
	member((PHId, PermitedUsers), PotentialHostsId),
	partition(call(member_for_call(PermitedUsers)), ViolationsIdIn,
		AttributedIds, ViolationsIdOut),
		length(AttributedIds, L), L\=0,
	attrib_host_aux1(PotentialHostsId, ViolationsIdOut,
		[(PHId, AttributedIds)|BindingsIn], BindingsOut).

attrib_host_aux0(_,_,_, NumHost):-
		max_number_host(Max), NumHost > Max, false.

attrib_host_aux0(ViolationsId, PotentialHostsId, Bindings, NumHost):-
	attrib_host_aux1(PotentialHostsId, ViolationsId, [], Bindings),
	(length(Bindings, L), L=<NumHost;
		NumHost1 is NumHost +1,
		attrib_host_aux0(ViolationsId, PotentialHostsId, Bindings, NumHost1)).

apply_bindings(Real, (HostId, WrongUsersId), GraphIn, GraphOut):-
    abstract(Real, GraphIn, Abs, G1),
    id_of_node(AbsId, Abs), put_contains(HostId, AbsId, G1, G2),
    id_of_node(RealId, Real),
    redirect_to_abstraction(WrongUsersId, RealId, AbsId, G2, GraphOut).

attrib_host(Real, ViolationsId, PotentialHosts, GraphIn, GraphOut):-
	attrib_host_aux0(ViolationsId, PotentialHosts, Bindings, 2),
    foldl(call(apply_bindings(Real)), Bindings, GraphIn, GraphOut).

%%%%%%%%%%%%

find_existing_abstractions(NodeId, GraphIn, GraphOut):-
    get_node(NodeId, GraphIn, Node),
    %%move to java_rules !!
    (kind_of_node(class, Node); kind_of_node(interface, Node))*->
    (get_abstractions(NodeId, GraphIn, KnownAbsIds)*->true;
        KnownAbsIds=[]),
    findall(SuperId, (subtype(GraphIn, NodeId, SuperId),
               NodeId\=SuperId, \+member(SuperId, KnownAbsIds)), SuperIds),
    foldl(call(add_abstraction(NodeId)), SuperIds, GraphIn, GraphOut);
    GraphIn=GraphOut.

%%%%

redirect_toward_aux([],_, X, X).
redirect_toward_aux([WrongUserId | WUsId], UseeId,
    (UnsolvedsIn, GraphIn), Out):-

    (gen_abstraction(UseeId, GraphIn, AbsId),
        \+is_violation(WrongUserId, AbsId, GraphIn)) *->
    redirect_to_abstraction([WrongUserId], UseeId, AbsId, GraphIn, G1),
    redirect_toward_aux(WUsId, UseeId, (UnsolvedsIn, G1), Out);
    redirect_toward_aux(WUsId, UseeId, ([WrongUserId | UnsolvedsIn], GraphIn), Out).

redirect_toward_known_abstractions(WrongUsersId, UseeId, GraphIn, Unsolveds, GraphOut):-
        redirect_toward_aux(WrongUsersId, UseeId, ([], GraphIn), (Unsolveds, GraphOut)).

%%%%
%% UseeId is either RealId or AbsId, tell us who use who
one_intro(Real, WrongUsersId, GraphIn, GraphOut):-
    kind_of_node(RealK, Real),
    abstract_kind(RealK, AbsK),
    find_host(Real, AbsK, WrongUsersId, GraphIn, HostId),
    abstract(Real, GraphIn, Abs, G1),
    id_of_node(AbsId, Abs),
    put_contains(HostId, AbsId, G1, G2),
    id_of_node(RealId, Real),
    redirect_to_abstraction(WrongUsersId, RealId, AbsId, G2, GraphOut).

multiple_intro(Real, WrongUsersId , GraphIn, GraphOut):-
    potential_host_set(Real, WrongUsersId, GraphIn, PHSet),!,
    attrib_host(Real, WrongUsersId, PHSet, GraphIn, GraphOut).

intro(_, [], G, G):-!.
intro(NodeId, WrongUsersId, GraphIn, GraphOut):-
    get_node(NodeId, GraphIn, Node),
    ((one_intro(Node, WrongUsersId, GraphIn, GraphOut)*->true;
        multiple_intro(Node, WrongUsersId, GraphIn, GraphOut))*->true;
    %%intro abs + host
    abstract(Node, GraphIn, Abs, G2),
    create_host(Abs, G2, Host, G3),
    id_of_node(AbsId, Abs), id_of_node(HostId, Host),
    put_contains(HostId, AbsId, G3, G4),
    kind_of_node(HostK, Host),
    find_host(Node, HostK, WrongUsersId, G4, HHostId),
    put_contains(HHostId, HostId, G4, G5),
    redirect_to_abstraction(WrongUsersId, NodeId, AbsId, G5, GraphOut)).




%%%%
solve_violations_toward(UseeId, GraphIn, GraphOut):-
    findall(UserId, violation(UserId,UseeId, GraphIn), WrongUsersId),
    find_existing_abstractions(UseeId, GraphIn, G1),
    redirect_toward_known_abstractions(WrongUsersId, UseeId, G1, RemainingWrongUsersId, G2),
    intro(UseeId, RemainingWrongUsersId, G2, GraphOut).

%%
%%solve(+GraphIn, +Constraints, -GraphOut)
%%

print_vtrace(G, NumCall, NextNumCall):-
    atomic_list_concat(['visual_trace', NumCall,  '.dot'], FileName),
    pl2dot('tmp.dot',G,[]),
    find_violations(G, Vs, GTmp),
    pl2dot(FileName, GTmp, Vs),
    NextNumCall is NumCall + 1.

solve(GraphIn, GraphOut):-
    violations_node_type_priority(Priority),
    solve(GraphIn, Priority, 1, GraphOut).

solve(GraphIn, Priority, NumCall, GraphOut) :-
    (prioritized_violation(Priority, P1, _, UseeId, GraphIn) *->

			  solve_violations_toward(UseeId, GraphIn, GraphO1),
     print_vtrace(GraphO1, NumCall, NextNumCall),
     solve(GraphO1, P1, NextNumCall, GraphOut);

     GraphOut=GraphIn).

prioritized_violation([], [], UserId, UseeId, G):-!, violation(UserId, UseeId, G).

prioritized_violation([P|Priority], [P|Priority], UserId, UseeId, G):-
    kind_of_node(P, Usee),
    gen_node(UseeId, G, Usee),
    violation(UserId, UseeId, G).

prioritized_violation([P|Priority], PriorityOut, UserId, UseeId, G):-
    forall((kind_of_node(P, Usee), gen_node(UseeId0, G, Usee)),
	    \+violation(_, UseeId0, G)), prioritized_violation(Priority, PriorityOut, UserId, UseeId, G).


violation(UserId, UseeId, G):-
    uses(UserId, UseeId, G),
    is_violation(UserId, UseeId, G).

%%%%
find_violations(GraphIn, Violations, GraphOut):- find_violations(GraphIn, [], Violations, GraphOut), !.

find_violations(GraphIn, Acc, Violations, GraphOut):-
    select_violation(GraphIn, Use, GraphO1) *->
	     find_violations(GraphO1, [ Use | Acc ], Violations, GraphOut);
    Violations=Acc, GraphOut= GraphIn,!.

select_violation(GraphIn, Use, GraphOut):-
    select_uses(UserId, UseeId, GraphIn, GraphOut),
    is_violation(UserId, UseeId, GraphOut),
    ids_to_use(UserId, UseeId, Use).




