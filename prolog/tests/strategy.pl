node(0, package, 'strategy', '').
node(1, package, 'candidate', '').
edge(contains,0,1).
node(11, class, 'StrategyDemo', '').
edge(contains,1,11).
node(3, method, 'main', '__java.lang.String').
edge(contains,11,3).
node(5, constructor, 'SortingContext', '#_@primitive.int_@primitive.int').
edge(uses,3,5).
node(2, class, 'SortingContext', '').
edge(uses,3,2).
node(12, constructor, 'StrategyDemo', '#_void').
edge(contains,11,12).
edge(contains,1,2).
node(8, method, 'bubbleSort', '__@primitive.int').
edge(contains,2,8).
node(7, method, 'exchange', '__@primitive.int_@primitive.int_@primitive.int').
edge(uses,8,7).
node(10, method, 'sort', '__@primitive.int_@primitive.int').
edge(contains,2,10).
edge(uses,10,8).
node(9, method, 'linearSort', '__@primitive.int').
edge(uses,10,9).
edge(contains,2,5).
edge(uses,5,10).
node(6, method, 'show', '__@primitive.int').
edge(uses,5,6).
edge(contains,2,9).
edge(uses,9,7).
edge(contains,2,7).
node(4, method, 'methodUnrelatedWithTheSorting', '__void').
edge(contains,2,4).
edge(contains,2,6).
