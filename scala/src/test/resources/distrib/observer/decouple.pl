java_import(['observer.candidate']).

declareSet(concreteObservers, ['DivObs', 'ModObs']).
declareSet(subject, ['Subject']).

hideScopeSetFrom(concreteObservers, subject).
