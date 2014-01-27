node(0, package, 'decorator', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(18, class, 'AwithY', '').
edge(contains,1,18).
node(11, constructor, 'AwithY', '#_void').
edge(contains,18,11).
node(8, constructor, 'A', '#_void').
edge(uses,11,8).
node(6, method, 'doIt', '__void').
edge(contains,18,6).
node(4, method, 'doIt', '__void').
edge(uses,6,4).
node(21, method, 'doY', '__void').
edge(uses,6,21).
edge(contains,18,21).
node(2, class, 'A', '').
edge(isa,18,2).
edge(uses,18,2).
edge(contains,1,2).
edge(contains,2,8).
edge(contains,2,4).
node(22, class, 'AwithZ', '').
edge(contains,1,22).
node(7, method, 'doIt', '__void').
edge(contains,22,7).
node(24, method, 'doZ', '__void').
edge(uses,7,24).
edge(uses,7,4).
edge(contains,22,24).
node(10, constructor, 'AwithZ', '#_void').
edge(contains,22,10).
edge(uses,10,8).
edge(isa,22,2).
edge(uses,22,2).
node(25, class, 'AwithXY', '').
edge(contains,1,25).
node(16, constructor, 'AwithXY', '#_void').
edge(contains,25,16).
node(9, constructor, 'AwithX', '#_void').
edge(uses,16,9).
node(13, method, 'doIt', '__void').
edge(contains,25,13).
node(5, method, 'doIt', '__void').
edge(uses,13,5).
node(19, attribute, 'obj', '').
edge(uses,13,19).
edge(uses,13,21).
edge(contains,25,19).
edge(uses,19,11).
edge(uses,19,18).
node(12, class, 'AwithX', '').
edge(isa,25,12).
edge(uses,25,12).
node(27, class, 'DecoratorDemo', '').
edge(contains,1,27).
node(3, method, 'main', '__java.lang.String').
edge(contains,27,3).
edge(uses,3,2).
edge(uses,3,16).
edge(uses,3,9).
node(17, constructor, 'AwithXYZ', '#_void').
edge(uses,3,17).
edge(uses,3,25).
edge(uses,3,4).
node(26, class, 'AwithXYZ', '').
edge(uses,3,26).
edge(uses,3,12).
node(28, constructor, 'DecoratorDemo', '#_void').
edge(contains,27,28).
edge(contains,1,26).
edge(contains,26,17).
edge(uses,17,9).
node(20, attribute, 'obj1', '').
edge(contains,26,20).
edge(uses,20,11).
edge(uses,20,18).
node(23, attribute, 'obj2', '').
edge(contains,26,23).
edge(uses,23,22).
edge(uses,23,10).
node(14, method, 'doIt', '__void').
edge(contains,26,14).
edge(uses,14,5).
edge(uses,14,20).
edge(uses,14,23).
edge(uses,14,24).
edge(uses,14,21).
edge(isa,26,12).
edge(uses,26,12).
edge(contains,1,12).
edge(contains,12,5).
edge(uses,5,4).
node(15, method, 'doX', '__void').
edge(uses,5,15).
edge(contains,12,9).
edge(uses,9,8).
edge(contains,12,15).
edge(isa,12,2).
edge(uses,12,2).
