
:-module(solver, 
	 [solve/3,
	  solve/1,
	  find_graph_and_constraints/2,
	  find_constraints/1,
	  find_violations/4,
	  friend/4]).

:-use_module(library(assoc)).
:-use_module(graph).
:-use_module(pl2dot).
:-use_module(constraints).

:-reexport(graph,[find_graph/1]).
%:-reexport(pl2dot).


find_graph_and_constraints(Graph, Constraints):- 
    find_graph(Graph), find_constraints(FNCts),
    constraintsListFullName2Ids(FNCts, Constraints, Graph).    

find_constraints(FNCts):-
    findall(hideFrom(Hidee,Interloper), hideFrom(Hidee,Interloper), FNCts0),
    findall(isFriendOf(User,Usee), isFriendOf(User,Usee), FNCts, FNCts0).

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
%% on entry 2 cases :
%% -the node has no host 
%% -the node has already a host (it has to be changed otherwise
%% -                                        and it satisfies the newly studied constraint
%% -                                        and it does not satisfy the newly studied constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


potential_host(Node, hideFrom(HideeId, InterloperId), Graph, HostId):-
    can_contain(Host, Node),
    get_node(HostId, Host, Graph),
    \+'contains*'(HideeId, HostId, Graph), 
    \+'contains*'(InterloperId, HostId, Graph),
    % for some obvious reason Node cannot be hosting himself
    % we have to explicitly tell it to prolog, because since nobody contains him,
    % it cannot break any constraint !!
    Host\=Node.


%% find an acceptable existing host
%% Excluded stores the host previously excluded for the node(Uid,_,_)
host(Node, Graph, hideFrom(HideeId, InterloperId), Excluded, GraphOut, NewExcluded):-
    id_of_node(Id, Node), 
    \+contains(_,Id,Graph),
    potential_host(Node, hideFrom(HideeId, InterloperId), Graph, HostId),
    put_contains(HostId, Id, Graph, GraphOut),
    %% since nobody contains Node, this is the first call to host for him
    %% therefore the excluded list must be empty
    excluded(Id, Excluded, []), 
    put_assoc(Id, Excluded, [HideeId| [InterloperId]], NewExcluded).


host(Node, Graph, hideFrom(HideeId, InterloperId), Excluded, Graph, NewExcluded):-
    %%has already a host and and it satisfies the newly studied constraint
    %% -> no reason to move host
    id_of_node(Id,Node), contains(HostId, Id, Graph),
    \+'contains*'(HideeId, HostId, Graph), \+'contains*'(InterloperId, HostId, Graph),
    
    excluded(Id, Excluded, IdEx), 
    put_assoc(Id, Excluded, [  HideeId | [ InterloperId | IdEx] ], NewExcluded).



host(Node, Graph, hideFrom(HideeId, InterloperId), Excluded, GraphOut, NewExcluded):-
    id_of_node(Id, Node),
    select_contains(PreviousHostId, Id, Graph, Graph2),
    
    ('contains*'(HideeId, PreviousHostId, Graph); 'contains*'(InterloperId, PreviousHostId, Graph)),
    
    potential_host(Node, hideFrom(HideeId, InterloperId), Graph2, HostId),
    put_contains(HostId, Id, Graph2, GraphOut),
    
    %check if the host do not break a violation previously solved
    excluded(Id, Excluded, IdEx), member(Ex, IdEx), 
    \+'contains*'(Ex, HostId, Graph2),
    put_assoc(Id, Excluded, [  HideeId | [ InterloperId | IdEx] ], NewExcluded).

%%
%%intro(+Graph, +Violation, +RuleViolated, +Abstractions,
%%         -GraphOut, -Abstractions).
%%
%if alreadey abstracted, do not recreate it
intro(Graph,  NodeId, Abstractions, Graph, Abs, Use, Abstractions):- 
    get_assoc(NodeId, Abstractions, (AbsId,Use) ), get_node(AbsId, Abs, Graph).

intro(GIn, NodeId, Abstractions, GOut , Abs, Use, NewAbstractions ):-
    %% check if not previously introduced
    \+get_assoc(NodeId, Abstractions, _), get_node(NodeId, Node, GIn), 
    %build abstraction
    abstract(Node, GIn, Abs, Use, GOut), id_of_node(AbsId, Abs),

    put_assoc(NodeId, Abstractions, (AbsId, Use), NewAbstractions1),
    %% little hack (?) : an abstraction is its own abstraction otherwise,
    %% on a later iteration, instead of moving the abstraction it will create an abstraction's abstraction ... 
    %% and that can go on...
    ids_to_use(AbsId, AbsId, FakeUse),
    put_assoc(AbsId, NewAbstractions1, (AbsId, FakeUse), NewAbstractions).

%    put_assoc(NodeId, Abstractions, (AbsId, Use), NewAbstractions).

%%the input graph has already the uses (UserId,UseeId) removed
fold(GraphIn, UserId, UseeId, RuleViolated, Constraints, Abstractions, Excluded, 
     GraphOut3, NewAbstractions, NewExcluded) :- 
    intro(GraphIn, UseeId, Abstractions, GraphOut1, UseeAbs, AbsUse, NewAbstractions),
    host(UseeAbs, GraphOut1, RuleViolated, Excluded, GraphOut2, NewExcluded),
    id_of_node(AbsId, UseeAbs), 
    
    %check that the use introduced between the node and his abstraction does not violate a rule
    % this test can only be done once the abstraction has a host
    ids_to_use(AbsUser, AbsUsee, AbsUse), \+violation(GraphOut2, Constraints, AbsUser, AbsUsee, _, _),

    put_uses(UserId, AbsId, GraphOut2, GraphOut3).

%%
%%solve(+GraphIn, +Constraints, -GraphOut)
%%
solve(GraphIn, Constraints, GraphOut):- 
    empty_assoc(Excluded), empty_assoc(Abstractions),
    solve(GraphIn, Constraints, Abstractions, Excluded, GraphOut, _, _).

solve(GraphIn, Constraints, Abstractions, Excluded, GraphOut, AbsOut, NewExcluded) :-
    violation(GraphIn , Constraints, UserId, UseeId, ViolatedConstraint, GraphWithoutViolation),
    fold(GraphWithoutViolation, UserId, UseeId, ViolatedConstraint, Constraints,
	     Abstractions, Excluded, GraphO1, AbsOut1, Excluded1), 
    solve(GraphO1, Constraints, AbsOut1, Excluded1, GraphOut, AbsOut, NewExcluded).

solve(Graph, Constraints, Abs, Exc, Graph, Abs, Exc):- \+ violation(Graph, Constraints, _, _, _, _).

%%
%% alternative definition that create a dot at each step
%%
%% solve(GraphIn, Constraints, GraphOut):- 
%%     empty_assoc(Excluded), empty_assoc(Abstractions),
%%     solve(GraphIn, Constraints, Abstractions, Excluded, GraphOut, _, _, 1).

%% solve(GraphIn, Constraints, Abstractions, Excluded, GraphOut, AbsOut, NewExcluded, NumCall) :-
%%     violation(GraphIn , Constraints, UserId, UseeId, ViolatedConstraint, GraphWithoutViolation),
%%     fold(GraphWithoutViolation, UserId, UseeId, ViolatedConstraint, Constraints,
%% 	     Abstractions, Excluded, GraphO1, AbsOut1, Excluded1), 

%%     atom_concat('visual_trace', NumCall, VT), atom_concat(VT, '.dot', FileName),
%%     find_violations(GraphO1, Constraints, Vs, GTmp),
%%     pl2dot(FileName, GTmp, Vs),
%%     NextNumCall is NumCall + 1,

%%     solve(GraphO1, Constraints, AbsOut1, Excluded1, GraphOut, AbsOut, NewExcluded, NextNumCall).

%% solve(Graph, Constraints, Abs, Exc, Graph, Abs, Exc, _):- \+ violation(Graph, Constraints, _, _, _, _).



friend(UserId, UseeId, Graph, Constraints):-
    member(isFriendOf(UserAncestorId, UseeAncestorId), Constraints),
    'contains*'(UserAncestorId, UserId, Graph),
    'contains*'(UseeAncestorId, UseeId, Graph). 

%%
%% violation(+Graph, +Constraints, -Violation, -Cause, - Uses\Violation)
%%
violation(GraphIn, Constraints, 
	  UserId, UseeId, hideFrom(HideeId, InterloperId), GraphOut):-
    member(hideFrom(HideeId, InterloperId),Constraints),
    select_uses(UserId, UseeId, GraphIn, GraphOut),
    \+friend(UserId, UseeId, GraphOut, Constraints),
    'contains*'(HideeId, UseeId, GraphOut), 
    'contains*'(InterloperId,UserId, GraphOut).
    
find_violations(GraphIn, Constraints, Violations, GraphOut):- find_violations(GraphIn, Constraints, [], Violations, GraphOut).

find_violations(GraphIn, Constraints, Acc, Violations, GraphOut):-
    violation(GraphIn, Constraints, UserId, UseeId, _, GraphO1),
    ids_to_use(UserId, UseeId, Use),
    find_violations(GraphO1, Constraints, [ Use | Acc ], Violations, GraphOut).

find_violations(GraphIn, Constraints, Acc, Acc, GraphIn):-  \+ violation(GraphIn, Constraints, _, _, _, _).
