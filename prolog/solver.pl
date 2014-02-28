
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

gen_container_id(Node, HostId, Graph):-
    can_contain(Host, Node),
    gen_node(HostId, Graph, Host), 
    Host\=Node.

%%%
% we search a host for an abstraction who use its realisation
%% ex : delegate call
user_host(Abs, RealId, GraphIn, GraphOut):-

    collect_constraints(RealId, GraphIn, Cts),
    
    gen_container_id(Abs, HostId, GraphIn),

    \+interloper(HostId, Cts, GraphIn),
 
    collect_constraints(HostId, GraphIn, HCts),
    (empty_constraint(HCts);
    	forall((uses(UserId, RealId, GraphIn), is_violation(UserId, RealId, GraphIn)),
				\+interloper(UserId, HCts, GraphIn))),
 
    id_of_node(AbsId, Abs), 
    put_contains(HostId, AbsId, GraphIn, GraphOut).

%%%
% we search a host an abstraction used by its realisation
% ex: abstraction = interface, realisation = class
usee_host(Abs, RealId, GraphIn, GraphOut):-
    
    gen_container_id(Abs, HostId, GraphIn),

    collect_constraints(HostId, GraphIn, Cts),
    (empty_constraint(Cts);
	  	\+interloper(RealId, Cts, GraphIn),
     forall((uses(UserId, RealId, GraphIn), is_violation(UserId, RealId, GraphIn)),
		\+interloper(UserId, Cts, GraphIn))),

    id_of_node(AbsId, Abs), 
    put_contains(HostId, AbsId, GraphIn, GraphOut).


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
potential_host_set(Abs, RealId, UseeId, ViolationsId, Graph, PHSet):-
	findall(H, (can_contain(H, Abs), gen_node(_, Graph, H)), Hosts),
		(id_of_node(UseeId, Abs) ->
	maplist(call(permited_users_usee_abs(RealId, ViolationsId, Graph)), Hosts, PHSet);
	maplist(call(permited_users_usee_real(RealId, ViolationsId, Graph)), Hosts, PHSet)).

%% filter_aux(Pred, Elt, L, LOut):-
%% 	call(Pred, Elt)-> LOut = [Elt| L]; LOut=L.

%% filter(Pred, ListIn, ListOut):-
%% 	foldl(call(filter_aux(Pred)), ListIn, [], ListOut).


member_for_call(List, Elt):-member(Elt, List).


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
	
attrib_host(ViolationsId, PotentialHosts, Bindings):-
	attrib_host_aux0(ViolationsId, PotentialHosts, Bindings, 1).

%%%%%%%%%%%%
%%%%

%%
%%intro(+Graph, +Violation, +RuleViolated, +AbsAssocs,
%%         -GraphOut, -AbsAssocs).
%%
intro(GraphIn, NodeId, GraphOut):- 
    %if alreadey abstracted, do not recreate it
    get_node(NodeId, GraphIn, Node), 
     
    abstract(Node, GraphIn, Abs, UseeId, GraphOut1),
    %%find host for newly created abstraction
    %%if UseeId = AbsId
    (id_of_node(UseeId, Abs) *-> usee_host(Abs, NodeId, GraphOut1, GraphOut);
     user_host(Abs, NodeId, GraphOut1, GraphOut)).


%%the input graph has already the uses (UserId,UseeId) removed
fold(UserId, UseeId, GraphIn, GraphOut2):- 
    
    (get_abstractions(UseeId, GraphIn, _) *-> GraphOut1=GraphIn;
     (find_valid_abstraction(GraphIn, UseeId, GraphOut1);
      intro(GraphIn, UseeId, GraphOut1))),

    %% when redirecting a use from a class toward an interface,
    %% we need to redirect all method uses,
    %% i don't know if it is generalisable, but we try anyway:
    % /!\ si une méthode de la classe abstraite en interface avait déjà été abstraite !
    %%  -> on peut les chercher
    %%  -> on peut forcer à résoudre les violations d'abords sur les classes
    findall((UserDescId, UseeDescId), 
	    ('contains*'(UserId, UserDescId,GraphIn),
	     'contains*'(UseeId, UseeDescId,GraphIn),
	     uses(UserDescId, UseeDescId, GraphIn)), DescUses),
    redirect_to_abs(DescUses, GraphOut1, GraphOut2).

%%%%

find_existing_abstractions(NodeId, GraphIn, GraphOut):-
    get_node(NodeId, GraphIn, Node),
    %%move to java_rules !!
    (kind_of_node(class, Node); kind_of_node(interface, Node))*->
    get_abstractions(NodeId, GraphIn, KnownAbsIds),
    findall(SuperId, (subtype(GraphIn, NodeId, SuperId),
               NodeId\=SuperId, \+member(SuperId, KnownAbsIds)), SuperIds),
    foldl(call(add_abstraction(NodeId)), SuperIds, GraphIn, GraphOut);
    GraphIn=GraphOut.




redirect_toward_aux([],_, X, X).
redirect_toward_aux([WrongUserId | WUsId], UseeId, 
    (UnsolvedsIn, GraphIn), Out):-
    
    (gen_abstraction(UseeId, AbsId, GraphIn), 
        \+is_violation(WrongUserId, AbsId, GraphIn)) *->
    redirect_to_abstraction([WrongUserId], UseeId, AbsId, GraphIn, G1),
    redirect_toward_aux(WUsId, UseeId, (UnsolvedsIn, G1), Out);
    redirect_toward_aux(WUsId, UseeId, ([WrongUserId | UnsolvedsIn], GraphIn), Out).

redirect_toward_existing_abstractions(WrongUsersId, UseeId, GraphIn, Unsolveds, GraphOut):-
        redirect_toward_aux(WrongUsersId, UseeId, ([], GraphIn), (Unsolveds, GraphOut)).

%%%%

solve_violations_toward(UseeId, GraphIn, GraphOut):-
    findall(UserId, violation(UserId,UseeId, GraphIn), WrongUsers),
    find_existing_abstractions(UseeId, GraphIn, G1),
    redirect_toward_existing_abstractions(WrongUsers, UseeId, G1, RemainingWrongUsers, G2),
    %%TODO finish function
    %% intro one or multiple abstractions at the same time !!!
    .

%%
%%solve(+GraphIn, +Constraints, -GraphOut)
%%

print_vtrace(G, NumCall, NextNumCall):-
    atomic_list_concat(['visual_trace', NumCall,  '.dot'], FileName),
     find_violations(G, Vs, GTmp),
     pl2dot(FileName, GTmp, Vs),
     NextNumCall is NumCall + 1.

solve(GraphIn, GraphOut):- 
    violations_node_type_priority(Priority), 
    solve(GraphIn, Priority, 1, GraphOut).

solve(GraphIn, Priority, NumCall, GraphOut) :-
    (prioritized_violation(Priority, P1, UserId, UseeId, GraphIn) *->

			  fold(UserId, UseeId, GraphIn, GraphO1), 
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


    
    
