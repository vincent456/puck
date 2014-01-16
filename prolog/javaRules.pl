:-module(javaRules, 
       [can_contain/2,
	abstract_type/2,
	abstract_uses/5]).

can_contain(package, package).
can_contain(package, class).
can_contain(package, interface).
can_contain(class, class).
can_contain(class, method).
can_contain(interface, method).
%can_contain(method, param).


abstract_type(package, package). %% is it ?
abstract_type(interface,interface).
abstract_type(class, interface).
abstract_type(method, method). %% methodImpl methodSig ??

%% abstract_uses(+AbstractionType, +NodeId, +AbsId, -UserId, -UseeId)
abstract_uses(interface, NodeId, AbsId, NodeId, AbsId).
abstract_uses(_, NodeId, AbsId, AbsId, NodeId).

