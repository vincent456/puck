node('root.scope_b.interloper', 'interloper').
node('root.scope_a.sub_scope_a1', 'sub_scope_a1').
node('root.scope_a.sub_scope_a0', 'sub_scope_a0').
node('root.scope_b', 'scope_b').
node('root.scope_a', 'scope_a').
node('root', 'root').

contains('root', 'root.scope_a').
contains('root.scope_a', 'root.scope_a.sub_scope_a1').
contains('root.scope_a', 'root.scope_a.sub_scope_a0').

contains('root', 'root.scope_b').
contains('root.scope_b', 'root.scope_b.interloper').

uses('root.scope_b.interloper', 'root.scope_a.sub_scope_a0').

%% constraints

%hideFrom('root.scope_a', 'root.scope_b').

%hideFrom('root.scope_a.sub_scope_a0', 'root.scope_b').
%hideFrom('root.scope_a.sub_scope_a0', 'root.scope_b.interloper').
