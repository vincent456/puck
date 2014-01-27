node(5, object, 'interloper', '').
node(4, object, 'sub_scope_a1', '').
node(3, object, 'sub_scope_a0', '').
node(2, object, 'scope_b', '').
node(1, object, 'scope_a', '').
node(0, object, 'root', '').

edge(contains, 0, 1).
edge(contains, 1, 4).
edge(contains, 1, 3).
edge(contains, 0, 2).
edge(contains, 2, 5).

edge(uses, 5, 3).
