
:-module(solver, 
	 [solve/2,
	  solve/1,
	  find_violations/3,
	  solver_read/3]).

:-use_module(library(assoc)).
:-use_module(graph).
:-use_module(constraints).
:-reexport(constraints).
:-reexport(graph).
%:-reexport(pl2dot).

solver_read(GraphFile, ConstraintFile, Graph):-
    read_graph(GraphFile, G), read_constraints(ConstraintFile, G, Graph),!.

solve(CorrectedGraph):-
    find_graph_and_constraints(Graph, Cts),
    find_violations(Graph, Cts, Vs, G2), pl2dot('graph_before.dot', G2, Vs),
    solve(Graph, Cts, CorrectedGraph), pl2dot('graph_after.dot', CorrectedGraph).

excluded(Id, List, Value):- get_assoc(Id,List,Value).
excluded(Id, List, []):- \+get_assoc(Id,List,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% host
%% we search a host for an abstraction, the goal is to redirect a use of node to a use of abstraction
%% if the abstraction has just been introduced we select a host that does not violate the constraint
%% it the abstraction has already been introduced, before redirecting we must check if the abstraction 
%% does not violate the newly studied constraint : if it is the case we move the abstraction
%%
%% another solution would be to create another abstraction and use this one but we aim to introduce as 
%% few node as possible
%%
%% on entry 3 cases :
%% -the node has no host 
%% | the node has already a host 
%% -                                        and it satisfies the newly studied constraint
%% -                                        and it does not satisfy the newly studied constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


potential_host(Node, hideFrom(HideeId, InterloperId), Graph, HostId):-
    can_contain(Host, Node),
    gen_node(HostId, Host, Graph),
    \+'contains*'(HideeId, HostId, Graph), 
    \+'contains*'(InterloperId, HostId, Graph),
    % for some obvious reason Node cannot be hosting himself
    % we have to explicitly tell it to prolog, because since nobody contains him,
    % it cannot break any constraint !!
    Host\=Node.


host(Node, HideeId, GraphIn, GraphOut):-
    collect_constraints(HideeId, GraphIn, Cts),
    can_contain(Host, Node),
    gen_node(HostId, Host, GraphIn), 
    % for some obvious reason Node cannot be hosting himself
    % we have to explicitly tell it to prolog, because since nobody contains him,
    % it cannot break any constraint !!
    Host\=Node,
    \+interloper(HostId, Cts, GraphIn),
    id_of_node(Id, Node), 
    put_contains(HostId, Id, GraphIn, GraphOut).

%% %% find an acceptable existing host
%% %% Excluded stores the host previously excluded for the node(Uid,_,_)
%% host(Node, Graph, hideFrom(HideeId, InterloperId), Excluded, GraphOut, NewExcluded):-
%%     id_of_node(Id, Node), 
%%     root(Id,Graph),
    
%%     potential_host(Node, hideFrom(HideeId, InterloperId), Graph, HostId),
%%     put_contains(HostId, Id, Graph, GraphOut),
%%     %% since nobody contains Node, this is the first call to host for him
%%     %% therefore the excluded list must be empty
%%     excluded(Id, Excluded, []), 
%%     put_assoc(Id, Excluded, [HideeId| [InterloperId]], NewExcluded).


%% host(Node, Graph, hideFrom(HideeId, InterloperId), Excluded, Graph, NewExcluded):-
%%     %%has already a host and and it satisfies the newly studied constraint
%%     %% -> no reason to move host
%%     id_of_node(Id,Node), contains(HostId, Id, Graph),
%%     \+'contains*'(HideeId, HostId, Graph), \+'contains*'(InterloperId, HostId, Graph),
    
%%     excluded(Id, Excluded, IdEx), 
%%     put_assoc(Id, Excluded, [  HideeId | [ InterloperId | IdEx] ], NewExcluded).



%% host(Node, Graph, hideFrom(HideeId, InterloperId), Excluded, GraphOut, NewExcluded):-
%%     id_of_node(Id, Node),
%%     select_contains(PreviousHostId, Id, Graph, Graph2),
    
%%     ('contains*'(HideeId, PreviousHostId, Graph); 'contains*'(InterloperId, PreviousHostId, Graph)),
    
%%     potential_host(Node, hideFrom(HideeId, InterloperId), Graph2, HostId),
%%     put_contains(HostId, Id, Graph2, GraphOut),
    
%%     %check if the host do not break a violation previously solved
%%     excluded(Id, Excluded, IdEx), member(Ex, IdEx), 
%%     \+'contains*'(Ex, HostId, Graph2),
%%     put_assoc(Id, Excluded, [  HideeId | [ InterloperId | IdEx] ], NewExcluded).

%%
%%intro(+Graph, +Violation, +RuleViolated, +AbsAssocs,
%%         -GraphOut, -AbsAssocs).
%%
intro(GraphIn, NodeId, AbsAssocsIn, GraphOut, Abs, AbsAssocsOut):- 
    %if alreadey abstracted, do not recreate it
    get_assoc(NodeId, AbsAssocsIn, AbsId) *-> 
	     get_node(AbsId, Abs, GraphIn), GraphOut=GraphIn, AbsAssocsOut=AbsAssocsIn;
    get_node(NodeId, Node, GraphIn), 
    abstract(Node, GraphIn, AbsAssocsIn, Abs, GraphOut, AbsAssocsOut).

redirect_to_abs_aux((UserId, UseeId) , (AbsAssocs, GraphIn), (AbsAssocs, GraphOut)) :-
    get_assoc(UseeId, AbsAssocs,AbsId) *->
	     select_uses(UserId, UseeId, GraphIn, G2),
    put_uses(UserId, AbsId, G2, GraphOut); 
    GraphIn=GraphOut.

redirect_to_abs(UsesList, AbsAssocs, GraphIn, GraphOut):-
    foldl(redirect_to_abs_aux, UsesList, (AbsAssocs, GraphIn), (_, GraphOut)).


%%the input graph has already the uses (UserId,UseeId) removed
fold(Use, GraphIn, AbsAssocs, GraphOut4, NewAbsAssocs) :- 
    ids_to_use(UseeId,UserId, Use),
    intro(GraphIn, UseeId, AbsAssocs, GraphOut1, UseeAbs, NewAbsAssocs),

    host(UseeAbs, UseeId, GraphOut1, GraphOut2),
    id_of_node(AbsId, UseeAbs), 
    
    %check that the use introduced between the node and his abstraction does not violate a rule
    % this test can only be done once the abstraction has a host
    (uses(AbsId, UseeId, GraphOut2), \+is_violation(AbsId, UseeId, GraphOut2);
     uses(UseeId, AbsId, GraphOut2), \+is_violation(UseeId, AbsId, GraphOut2)),
    
    put_uses(UserId, AbsId, GraphOut2, GraphOut3),
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
    redirect_to_abs(DescUses, AbsAssocs, GraphOut3, GraphOut4).

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


solve(GraphIn, GraphOut):- 
    empty_assoc(AbsAssocs), solve(GraphIn, AbsAssocs, GraphOut, _, 1).

solve(GraphIn, AbsAssocs, GraphOut, AbsOut, NumCall) :-
    (violation(GraphIn , Use, GraphWithoutViolation) *->
	      fold(Use, GraphWithoutViolation, AbsAssocs, GraphO1, AbsOut1), 
     
     atom_concat('visual_trace', NumCall, VT), atom_concat(VT, '.dot', FileName),

     find_violations(GraphO1, Vs, GTmp),
     pl2dot(FileName, GTmp, Vs),
     NextNumCall is NumCall + 1,
     
     solve(GraphO1, AbsOut1, GraphOut, AbsOut, NextNumCall);
     GraphOut=GraphIn, AbsOut = AbsAssocs).


%%
%% violation(+Graph, +Constraints, -Violation, -Cause, - Uses\Violation)
%%

find_violations(GraphIn, Violations, GraphOut):- find_violations(GraphIn, [], Violations, GraphOut), !.

find_violations(GraphIn, Acc, Violations, GraphOut):-
    violation(GraphIn, Use, GraphO1) *->
	     find_violations(GraphO1, [ Use | Acc ], Violations, GraphOut);
    Violations=Acc, GraphOut= GraphIn,!.

violation(GraphIn, Use, GraphOut):-
    select_uses(UserId, UseeId, GraphIn, GraphOut),
    is_violation(UserId, UseeId, GraphOut), 
    ids_to_use(UserId, UseeId, Use).
    
    
