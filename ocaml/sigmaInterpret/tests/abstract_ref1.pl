:-ensure_loaded('/home/lorilan/thesis/projects/sigmaInterpret/pl_eval/evaluator.pl').
:-ensure_loaded('abstract.decouple.pl').
node('root.scope_b.sub_scope_b',object, 'sub_scope_b').
node('root.scope_a.sub_scope_a0_indirection',object, 'sub_scope_a0_indirection').
node('root.scope_b.sub_scope_b',object, 'sub_scope_b').
node('root.scope_a.sub_scope_a1',object, 'sub_scope_a1').
node('root.scope_a.sub_scope_a0',object, 'sub_scope_a0').
node('root.scope_a.sub_scope_a0_indirection',object, 'sub_scope_a0_indirection').
node('root.scope_a.sub_scope_a0',object, 'sub_scope_a0').
node('root.scope_b',object, 'scope_b').
node('root.scope_a',object, 'scope_a').
node('root',object, 'root').
edge(uses, 'root', 'root.scope_b.sub_scope_b').
edge(uses, 'root.scope_b.sub_scope_b', 'root.scope_a.sub_scope_a0_indirection').
edge(contains, 'root.scope_b', 'root.scope_b.sub_scope_b').
edge(contains, 'root.scope_a', 'root.scope_a.sub_scope_a1').
edge(uses, 'root.scope_a.sub_scope_a0_indirection', 'root.scope_a.sub_scope_a0').
edge(contains, 'root.scope_a', 'root.scope_a.sub_scope_a0_indirection').
edge(contains, 'root.scope_a', 'root.scope_a.sub_scope_a0').
edge(contains, 'root', 'root.scope_b').
edge(contains, 'root', 'root.scope_a').
:-pl2dot.
:-halt.
