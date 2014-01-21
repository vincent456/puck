:- use_module('../solver').
:- discontiguous node/4, edge/3, hideFrom/2, isFriendOf/2.

node(0, package, 'prototype', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(16, class, 'PrototypeDemo', '').
edge(contains,1,16).
node(3, attribute, 'roles', '').
edge(contains,16,3).
node(2, interface, 'Stooge', '').
edge(uses,3,2).
node(4, method, 'main', '__java.lang.String').
edge(contains,16,4).
edge(uses,4,3).
edge(uses,4,2).
node(5, method, 'slap_stick', '__void').
edge(uses,4,5).
node(7, method, 'user_creator', '__@primitive.int').
edge(uses,4,7).
node(17, constructor, 'PrototypeDemo', '#_void').
edge(contains,16,17).
edge(contains,16,7).
edge(uses,7,3).
node(9, constructor, 'Larry', '#_void').
edge(uses,7,9).
node(13, class, 'Curly', '').
edge(uses,7,13).
node(10, class, 'Moe', '').
edge(uses,7,10).
node(6, class, 'Larry', '').
edge(uses,7,6).
node(15, constructor, 'Curly', '#_void').
edge(uses,7,15).
node(12, constructor, 'Moe', '#_void').
edge(uses,7,12).
edge(contains,1,2).
edge(contains,2,5).
edge(contains,1,13).
node(14, method, 'slap_stick', '__void').
edge(contains,13,14).
edge(contains,13,15).
edge(isa,13,2).
edge(uses,13,2).
edge(contains,1,10).
node(11, method, 'slap_stick', '__void').
edge(contains,10,11).
edge(contains,10,12).
edge(isa,10,2).
edge(uses,10,2).
edge(contains,1,6).
edge(contains,6,9).
node(8, method, 'slap_stick', '__void').
edge(contains,6,8).
edge(isa,6,2).
edge(uses,6,2).

hideFrom('prototype.candidate.Curly', 'prototype'). %%hideFrom root -> hideFrom everybody
hideFrom('prototype.candidate.Moe', 'prototype').
hideFrom('prototype.candidate.Larry', 'prototype').

isFriendOf('prototype.candidate.PrototypeDemo.main__java.lang.String', 'prototype.candidate.Curly').
isFriendOf('prototype.candidate.PrototypeDemo.main__java.lang.String', 'prototype.candidate.Moe').
isFriendOf('prototype.candidate.PrototypeDemo.main__java.lang.String', 'prototype.candidate.Larry').

%:-solve(_), halt.
