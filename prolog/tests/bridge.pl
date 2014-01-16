:- use_module('../solver').
:- discontiguous node/4, edge/3, hideFrom/2, isFriendOf/2.

node(0, package, 'bridge2', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(100, class, 'BridgeDisc', '').
edge(contains,1,100).
node(3, method, 'main', '__java.lang.String').
edge(contains,100,3).
node(46, constructor, 'StackHanoi', '#_void').
edge(uses,3,46).
node(93, method, 'reportRejected', '__void').
edge(uses,3,93).
node(60, method, 'isEmpty', '__void').
edge(uses,3,60).
node(8, method, 'push', '__@primitive.int').
edge(uses,3,8).
node(44, constructor, 'StackArray', '#_void').
edge(uses,3,44).
node(2, class, 'StackArray', '').
edge(uses,3,2).
node(10, method, 'pop', '__void').
edge(uses,3,10).
node(77, class, 'StackFIFO', '').
edge(uses,3,77).
node(62, method, 'pop', '__void').
edge(uses,3,62).
node(47, constructor, 'StackFIFO', '#_void').
edge(uses,3,47).
node(49, class, 'StackList', '').
edge(uses,3,49).
node(53, method, 'push', '__@primitive.int').
edge(uses,3,53).
node(75, constructor, 'StackList', '#_void').
edge(uses,3,75).
node(88, class, 'StackHanoi', '').
edge(uses,3,88).
node(13, method, 'isEmpty', '__void').
edge(uses,3,13).
node(102, constructor, 'BridgeDisc', '#_void').
edge(contains,100,102).
edge(contains,1,49).
node(61, method, 'top', '__void').
edge(contains,49,61).
node(109, attribute, 'value', '').
edge(uses,61,109).
node(52, attribute, 'last', '').
edge(uses,61,52).
edge(uses,61,60).
edge(contains,49,53).
node(112, constructor, 'Node', '#_@primitive.int').
edge(uses,53,112).
edge(uses,53,52).
node(106, attribute, 'prev', '').
edge(uses,53,106).
node(108, attribute, 'next', '').
edge(uses,53,108).
node(103, class, 'Node', '').
edge(uses,53,103).
edge(contains,49,52).
edge(uses,52,103).
edge(contains,49,60).
edge(uses,60,52).
edge(contains,49,75).
node(71, method, 'isFull', '__void').
edge(contains,49,71).
edge(contains,49,62).
edge(uses,62,109).
edge(uses,62,52).
edge(uses,62,106).
edge(uses,62,60).
edge(contains,1,88).
edge(contains,88,46).
edge(uses,46,44).
edge(contains,88,93).
node(92, attribute, 'totalRejected', '').
edge(uses,93,92).
edge(contains,88,92).
node(20, method, 'push', '__@primitive.int').
edge(contains,88,20).
edge(uses,20,92).
node(9, method, 'top', '__void').
edge(uses,20,9).
edge(uses,20,8).
edge(uses,20,13).
%edge(isa,88,2).
edge(uses,88,2).
edge(contains,1,2).
edge(contains,2,9).
edge(uses,9,13).
node(7, attribute, 'items', '').
edge(uses,9,7).
node(11, attribute, 'total', '').
edge(uses,9,11).
edge(contains,2,8).
node(14, method, 'isFull', '__void').
edge(uses,8,14).
edge(uses,8,7).
edge(uses,8,11).
edge(contains,2,14).
edge(uses,14,11).
edge(contains,2,44).
edge(contains,2,13).
edge(uses,13,11).
edge(contains,2,10).
edge(uses,10,13).
edge(uses,10,7).
edge(uses,10,11).
edge(contains,2,7).
edge(contains,2,11).
edge(contains,1,77).
node(4, attribute, 'temp', '').
edge(contains,77,4).
edge(uses,4,44).
edge(uses,4,2).
node(18, method, 'pop', '__void').
edge(contains,77,18).
edge(uses,18,4).
edge(uses,18,8).
edge(uses,18,13).
edge(uses,18,10).
edge(contains,77,47).
edge(uses,47,44).
%edge(isa,77,2).
edge(uses,77,2).
edge(contains,1,103).
edge(contains,103,109).
edge(contains,103,112).
edge(uses,112,109).
edge(contains,103,106).
edge(uses,106,103).
edge(contains,103,108).
edge(uses,108,103).

%%hideScopeSetFrom(refinedAbstractions, implementations).
hideFrom('bridge2.candidate.StackFIFO', 'bridge2.candidate.StackArray').
hideFrom('bridge2.candidate.StackFIFO', 'bridge2.candidate.StackList').
hideFrom('bridge2.candidate.StackHanoi', 'bridge2.candidate.StackArray').
hideFrom('bridge2.candidate.StackHanoi', 'bridge2.candidate.StackList').

%%hideScopeSetFrom(concreteImplementations, abstractions).
hideFrom('bridge2.candidate.StackArray', 'bridge2.candidate.StackFIFO').
hideFrom('bridge2.candidate.StackArray', 'bridge2.candidate.StackHanoi').
hideFrom('bridge2.candidate.StackList', 'bridge2.candidate.StackFIFO').
hideFrom('bridge2.candidate.StackList', 'bridge2.candidate.StackHanoi').

:-solve(_), halt. 
