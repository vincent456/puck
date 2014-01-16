:-module(javaRules, 
       [can_contain/2,
	abstract_type/2,
	abstract_uses/5]).

can_contain(object, object).
can_contain(object,method).

abstract_type(object, object).
abstract_uses(_, NodeId, AbsId, AbsId, NodeId).
