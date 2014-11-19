java_import(['prototype.actors']).

declareSet(concrete_prototypes, ['Tragedian', 'Comedian', 'Extra']).

hideScopeSet(concrete_prototypes).

friendOfScope('PrototypeDemo.main__String[]', concrete_prototypes).

%hideScopeSetFrom(concrete_prototypes, 'PrototypeDemo.makeMovie__int').