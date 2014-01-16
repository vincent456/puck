:- use_module('../solver').
:- discontiguous node/4, edge/3, hideFrom/2, isFriendOf/2.

node(0, package, 'decorator', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(25, class, 'AwithY', '').
edge(contains,1,25).
node(12, constructor, 'AwithY', '#_void').
edge(contains,25,12).
node(9, constructor, 'A', '#_void').
edge(uses,12,9).
node(6, method, 'doIt', '__void').
edge(contains,25,6).
node(4, method, 'doIt', '__void').
edge(uses,6,4).
node(32, method, 'doY', '__void').
edge(uses,6,32).
edge(contains,25,32).
node(2, class, 'A', '').
%edge(isa,25,2).
edge(uses,25,2).
edge(contains,1,2).
edge(contains,2,9).
edge(contains,2,4).
node(39, class, 'AwithZ', '').
edge(contains,1,39).
node(7, method, 'doIt', '__void').
edge(contains,39,7).
node(44, method, 'doZ', '__void').
edge(uses,7,44).
edge(uses,7,4).
edge(contains,39,44).
node(10, constructor, 'AwithZ', '#_void').
edge(contains,39,10).
edge(uses,10,9).
%edge(isa,39,2).
edge(uses,39,2).
node(49, class, 'AwithXY', '').
edge(contains,1,49).
node(23, constructor, 'AwithXY', '#_void').
edge(contains,49,23).
node(11, constructor, 'AwithX', '#_void').
edge(uses,23,11).
node(17, method, 'doIt', '__void').
edge(contains,49,17).
node(5, method, 'doIt', '__void').
edge(uses,17,5).
node(27, attribute, 'obj', '').
edge(uses,17,27).
edge(uses,17,32).
edge(contains,49,27).
edge(uses,27,12).
edge(uses,27,25).
node(13, class, 'AwithX', '').
%edge(isa,49,13).
edge(uses,49,13).
node(67, class, 'DecoratorDemo', '').
edge(contains,1,67).
node(3, method, 'main', '__java.lang.String').
edge(contains,67,3).
edge(uses,3,2).
edge(uses,3,23).
edge(uses,3,11).
node(22, constructor, 'AwithXYZ', '#_void').
edge(uses,3,22).
edge(uses,3,49).
edge(uses,3,4).
node(57, class, 'AwithXYZ', '').
edge(uses,3,57).
edge(uses,3,13).
node(69, constructor, 'DecoratorDemo', '#_void').
edge(contains,67,69).
edge(contains,1,57).
edge(contains,57,22).
edge(uses,22,11).
node(28, attribute, 'obj1', '').
edge(contains,57,28).
edge(uses,28,12).
edge(uses,28,25).
node(41, attribute, 'obj2', '').
edge(contains,57,41).
edge(uses,41,39).
edge(uses,41,10).
node(18, method, 'doIt', '__void').
edge(contains,57,18).
edge(uses,18,5).
edge(uses,18,28).
edge(uses,18,41).
edge(uses,18,44).
edge(uses,18,32).
%edge(isa,57,13).
edge(uses,57,13).
edge(contains,1,13).
edge(contains,13,5).
edge(uses,5,4).
node(19, method, 'doX', '__void').
edge(uses,5,19).
edge(contains,13,11).
edge(uses,11,9).
edge(contains,13,19).
%edge(isa,13,2).
edge(uses,13,2).

hideFrom('decorator.candidate.A', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithZ'). 
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.A', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithX', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.AwithX', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithY', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.AwithY', 'decorator.candidate.AwithXYZ').
 
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithZ','decorator.candidate.AwithXY'). 
hideFrom('decorator.candidate.AwithZ', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithXY', 'decorator.candidate.AwithXYZ').

hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.A').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithX').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithY').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithZ').
hideFrom('decorator.candidate.AwithXYZ', 'decorator.candidate.AwithXY'). 

:-solve(_),halt. 
