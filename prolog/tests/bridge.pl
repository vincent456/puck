node(0, package, 'bridge2', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(29, class, 'BridgeDisc', '').
edge(contains,1,29).
node(4, method, 'main', arrow('String[]','@primitive.void')).
edge(contains,29,4).
node(15, constructor, 'StackHanoi', arrow('@primitive.void','bridge2.candidate.StackHanoi')).
edge(uses,4,15).
node(28, method, 'reportRejected', arrow('@primitive.void','@primitive.int')).
edge(uses,4,28).
node(20, method, 'isEmpty', arrow('@primitive.void','@primitive.boolean')).
edge(uses,4,20).
node(6, method, 'push', arrow('@primitive.int','@primitive.void')).
edge(uses,4,6).
node(14, constructor, 'StackArray', arrow('@primitive.void','bridge2.candidate.StackArray')).
edge(uses,4,14).
node(2, class, 'StackArray', '').
edge(uses,4,2).
node(8, method, 'pop', arrow('@primitive.void','@primitive.int')).
edge(uses,4,8).
node(25, class, 'StackFIFO', '').
edge(uses,4,25).
node(22, method, 'pop', arrow('@primitive.void','@primitive.int')).
edge(uses,4,22).
node(16, constructor, 'StackFIFO', arrow('@primitive.void','bridge2.candidate.StackFIFO')).
edge(uses,4,16).
node(17, class, 'StackList', '').
edge(uses,4,17).
node(19, method, 'push', arrow('@primitive.int','@primitive.void')).
edge(uses,4,19).
node(24, constructor, 'StackList', arrow('@primitive.void','bridge2.candidate.StackList')).
edge(uses,4,24).
node(26, class, 'StackHanoi', '').
edge(uses,4,26).
node(10, method, 'isEmpty', arrow('@primitive.void','@primitive.boolean')).
edge(uses,4,10).
node(30, constructor, 'BridgeDisc', arrow('@primitive.void','bridge2.candidate.BridgeDisc')).
edge(contains,29,30).
edge(contains,1,17).
node(21, method, 'top', arrow('@primitive.void','@primitive.int')).
edge(contains,17,21).
node(34, attribute, 'value', '@primitive.int').
edge(uses,21,34).
node(18, attribute, 'last', 'bridge2.candidate.Node').
edge(uses,21,18).
edge(uses,21,20).
edge(contains,17,19).
node(35, constructor, 'Node', arrow('@primitive.int','bridge2.candidate.Node')).
edge(uses,19,35).
edge(uses,19,18).
node(32, attribute, 'prev', 'bridge2.candidate.Node').
edge(uses,19,32).
node(33, attribute, 'next', 'bridge2.candidate.Node').
edge(uses,19,33).
node(31, class, 'Node', '').
edge(uses,19,31).
edge(contains,17,18).
edge(uses,18,31).
edge(contains,17,20).
edge(uses,20,18).
edge(contains,17,24).
node(23, method, 'isFull', arrow('@primitive.void','@primitive.boolean')).
edge(contains,17,23).
edge(contains,17,22).
edge(uses,22,34).
edge(uses,22,18).
edge(uses,22,32).
edge(uses,22,20).
edge(contains,1,26).
edge(contains,26,15).
edge(uses,15,14).
edge(contains,26,28).
node(27, attribute, 'totalRejected', '@primitive.int').
edge(uses,28,27).
edge(contains,26,27).
node(13, method, 'push', arrow('@primitive.int','@primitive.void')).
edge(contains,26,13).
edge(uses,13,27).
node(7, method, 'top', arrow('@primitive.void','@primitive.int')).
edge(uses,13,7).
edge(uses,13,6).
edge(uses,13,10).
edge(isa,26,2).
edge(uses,26,2).
edge(contains,1,2).
edge(contains,2,7).
edge(uses,7,10).
node(5, attribute, 'items', 'int[]').
edge(uses,7,5).
node(9, attribute, 'total', '@primitive.int').
edge(uses,7,9).
edge(contains,2,6).
node(11, method, 'isFull', arrow('@primitive.void','@primitive.boolean')).
edge(uses,6,11).
edge(uses,6,5).
edge(uses,6,9).
edge(contains,2,11).
edge(uses,11,9).
edge(contains,2,14).
edge(contains,2,10).
edge(uses,10,9).
edge(contains,2,8).
edge(uses,8,10).
edge(uses,8,5).
edge(uses,8,9).
edge(contains,2,5).
edge(contains,2,9).
edge(contains,1,25).
node(3, attribute, 'temp', 'bridge2.candidate.StackArray').
edge(contains,25,3).
edge(uses,3,14).
edge(uses,3,2).
node(12, method, 'pop', arrow('@primitive.void','@primitive.int')).
edge(contains,25,12).
edge(uses,12,3).
edge(uses,12,6).
edge(uses,12,10).
edge(uses,12,8).
edge(contains,25,16).
edge(uses,16,14).
edge(isa,25,2).
edge(uses,25,2).
edge(contains,1,31).
edge(contains,31,34).
edge(contains,31,35).
edge(uses,35,34).
edge(contains,31,32).
edge(uses,32,31).
edge(contains,31,33).
edge(uses,33,31).
uses_dependency((4, 22), (4, 17)).
uses_dependency((12, 6), (3, 2)).
uses_dependency((12, 8), (3, 2)).
uses_dependency((4, 19), (4, 17)).
uses_dependency((12, 10), (3, 2)).
uses_dependency((4, 20), (4, 17)).
uses_dependency((12, 8), (3, 2)).
