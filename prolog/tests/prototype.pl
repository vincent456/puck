node(0, package, 'prototype', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(16, class, 'PrototypeDemo', '').
edge(contains,1,16).
node(4, attribute, 'roles', 'java.util.List<prototype.candidate.Stooge>').
edge(contains,16,4).
node(2, interface, 'Stooge', '').
edge(uses,4,2).
node(3, method, 'main', (['String[]'],'@primitive.void')).
edge(contains,16,3).
edge(uses,3,4).
edge(uses,3,2).
node(5, method, 'slap_stick', (['@primitive.void'],'@primitive.void')).
edge(uses,3,5).
node(7, method, 'user_creator', (['@primitive.int'],'@primitive.void')).
edge(uses,3,7).
node(17, constructor, 'PrototypeDemo', (['@primitive.void'],'prototype.candidate.PrototypeDemo')).
edge(contains,16,17).
edge(contains,16,7).
edge(uses,7,4).
node(9, constructor, 'Larry', (['@primitive.void'],'prototype.candidate.Larry')).
edge(uses,7,9).
node(13, class, 'Curly', '').
edge(uses,7,13).
node(10, class, 'Moe', '').
edge(uses,7,10).
node(6, class, 'Larry', '').
edge(uses,7,6).
node(15, constructor, 'Curly', (['@primitive.void'],'prototype.candidate.Curly')).
edge(uses,7,15).
node(12, constructor, 'Moe', (['@primitive.void'],'prototype.candidate.Moe')).
edge(uses,7,12).
edge(contains,1,2).
edge(contains,2,5).
edge(contains,1,13).
node(14, method, 'slap_stick', (['@primitive.void'],'@primitive.void')).
edge(contains,13,14).
edge(contains,13,15).
edge(isa,13,2).
edge(uses,13,2).
edge(contains,1,10).
node(11, method, 'slap_stick', (['@primitive.void'],'@primitive.void')).
edge(contains,10,11).
edge(contains,10,12).
edge(isa,10,2).
edge(uses,10,2).
edge(contains,1,6).
edge(contains,6,9).
node(8, method, 'slap_stick', (['@primitive.void'],'@primitive.void')).
edge(contains,6,8).
edge(isa,6,2).
edge(uses,6,2).
node(18,class,'Creator','').
edge(contains, 1, 18).
