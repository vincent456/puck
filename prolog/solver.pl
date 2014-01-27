
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
intro(GraphIn, NodeId, AbsAssocsIn, GraphOut, AbsAssocsOut):- 
    %if alreadey abstracted, do not recreate it
    get_assoc(NodeId, AbsAssocsIn, _) *-> GraphOut=GraphIn, AbsAssocsOut=AbsAssocsIn;
    
    (get_node(NodeId, Node, GraphIn), 
    abstract(Node, GraphIn, AbsAssocsIn, Abs, UseeId, GraphOut1, AbsAssocsOut),
     
    %%if UseeId = AbsId
    (id_of_node(UseeId, Abs) *-> usee_host(Abs, NodeId, GraphOut1, GraphOut);
     user_host(Abs, NodeId, GraphOut1, GraphOut))).


redirect_to_abs_aux((UserId, UseeId) , (AbsAssocs, GraphIn), (AbsAssocs, GraphOut)) :-
    get_assoc(UseeId, AbsAssocs,AbsId) *->
	     select_uses(UserId, UseeId, GraphIn, G2),
    put_uses(UserId, AbsId, G2, GraphOut); 
    GraphIn=GraphOut.

redirect_to_abs(UsesList, AbsAssocs, GraphIn, GraphOut):-
    foldl(redirect_to_abs_aux, UsesList, (AbsAssocs, GraphIn), (_, GraphOut)).


%%the input graph has already the uses (UserId,UseeId) removed
fold(UserId, UseeId, GraphIn, AbsAssocs, GraphOut2, NewAbsAssocs) :- 
    
    intro(GraphIn, UseeId, AbsAssocs, GraphOut1, NewAbsAssocs),

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
    redirect_to_abs(DescUses, AbsAssocs, GraphOut1, GraphOut2).

%%
%%solve(+GraphIn, +Constraints, -GraphOut)
%%
%% solve(GraphIn, GraphOut):- 
%%     empty_assoc(AbsAssocs), solve(GraphIn, AbsAssocs, GraphOut, _).

%% solve(GraphIn, AbsAssocs, GraphOut, AbsOut) :-
%%     (violation(GraphIn , Use, GraphWithoutViolation) *->
%% 	      fold(Use, GraphWithoutViolation, AbsAssocs, GraphO1, AbsOut1), 
%%      solve(GraphO1, AbsOut1, GraphOut, AbsOut);
%%      GraphOut=GraphIn, AbsOut = AbsAssocs).


print_vtrace(G, NumCall, NextNumCall):-
    atomic_list_concat(['visual_trace', NumCall,  '.dot'], FileName),
     find_violations(G, Vs, GTmp),
     pl2dot(FileName, GTmp, Vs),
     NextNumCall is NumCall + 1.


solve(GraphIn, GraphOut):- 
    empty_assoc(AbsAssocs), solve(GraphIn, AbsAssocs, GraphOut, _, 1).

solve(GraphIn, AbsAssocs, GraphOut, AbsOut, NumCall) :-
    (violation(UserId, UseeId, GraphIn) *->
	      fold(UserId, UseeId, GraphIn, AbsAssocs, GraphO1, AbsOut1), 
     
     print_vtrace(GraphO1, NumCall, NextNumCall),
     
     solve(GraphO1, AbsOut1, GraphOut, AbsOut, NextNumCall);
     GraphOut=GraphIn, AbsOut = AbsAssocs).


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


    
    
