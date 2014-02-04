
:-module(solver, 
	 [solve/2,
	  find_violations/3,
	  solver_read/3,
	  solver_read_and_solve/2]).

:-use_module(library(assoc)).
:-use_module(graph).
:-reexport(graph).
:-use_module(constraints).
:-reexport(constraints).

solver_read(GraphFile, ConstraintFile, Graph):-
    read_graph(GraphFile, G), read_constraints(ConstraintFile, G, Graph),!.

solver_read_and_solve(BaseName, G3):-
    atomic_list_concat([BaseName, '.pl'], GFile),
    atomic_list_concat([BaseName, '_decouple.pl'], DecoupleFile),
    atomic_list_concat([BaseName, '_before.dot'], BeforeFile),
    atomic_list_concat([BaseName, '_after.dot'], AfterFile),
    solver_read(GFile, DecoupleFile, G),
    find_violations(G, Vs, G2), pl2dot(BeforeFile, G2, Vs),
    solve(G, G3), pl2dot(AfterFile, G3).

excluded(Id, List, Value):- get_assoc(Id,List,Value).
excluded(Id, List, []):- \+get_assoc(Id,List,_).

gen_container_id(Node, HostId, Graph):-
    can_contain(Host, Node),
    gen_node(HostId, Host, Graph), 
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


%%
%%intro(+Graph, +Violation, +RuleViolated, +AbsAssocs,
%%         -GraphOut, -AbsAssocs).
%%
intro(GraphIn, NodeId, GraphOut):- 
    %if alreadey abstracted, do not recreate it
    get_abstractions(NodeId, _, GraphIn) *-> GraphOut=GraphIn;
    
    (get_node(NodeId, Node, GraphIn), 
     abstract(Node, GraphIn, Abs, UseeId, GraphOut1),
     
    %%if UseeId = AbsId
     (id_of_node(UseeId, Abs) *-> usee_host(Abs, NodeId, GraphOut1, GraphOut);
      user_host(Abs, NodeId, GraphOut1, GraphOut))).


redirect_to_abs_aux((UserId, UseeId) , GraphIn, GraphOut) :-
    get_abstractions(UseeId, [AbsId], GraphIn) *->
	     select_uses(UserId, UseeId, GraphIn, G2),
    put_uses(UserId, AbsId, G2, GraphOut); 
    GraphIn=GraphOut.

redirect_to_abs(UsesList, GraphIn, GraphOut):-
    foldl(redirect_to_abs_aux, UsesList, GraphIn, GraphOut).


%%the input graph has already the uses (UserId,UseeId) removed
fold(UserId, UseeId, GraphIn, GraphOut2) :- 
    
    intro(GraphIn, UseeId, GraphOut1),

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
    type_of_node(P, Usee),
    gen_node(UseeId, Usee, G), 
    violation(UserId, UseeId, G).

prioritized_violation([P|Priority], PriorityOut, UserId, UseeId, G):-
    forall((type_of_node(P, Usee), gen_node(UseeId0, Usee, G)),
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


    
    
