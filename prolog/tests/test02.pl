:- use_module('../solver').
:- discontiguous node/4, edge/3, hideFrom/2, isFriendOf/2.

node(0, object, 'root', '').

node(1, object, 'scope_a', '').
node(2, object, 'scope_b', '').

node(3, object, 'sub_scope_a0', '').
node(4, object, 'sub_scope_a1', '').

node(5, object, 'interloper', '').
node(6, object, 'friend', '').

edge(contains, 0, 1).
edge(contains, 1, 3).
edge(contains, 1, 4).

edge(contains, 0, 2).
edge(contains, 2, 5).
edge(contains, 2, 6).
    
edge(uses, 5, 3).
edge(uses, 6, 3).


%% constraints
%hideFrom('root.scope_a', 'root.scope_b').

hideFrom('root.scope_a.sub_scope_a0', 'root.scope_b').
%hideFrom('root.scope_a.sub_scope_a0', 'root.scope_b.interloper').

isFriendOf('root.scope_b.friend', 'root.scope_a').

%% :-solve(CorrectedGraph), halt. 

:-find_graph_and_constraints(Graph,Cts), 
  findall(CG, solve(Graph,Cts, CG), CGs), length(CGs, L), halt.

