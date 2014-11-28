java_import(['fileSystem', 
	'fileSystem.Directory', 'fileSystem.File']).

declareSet(uniformMethods, 
	['ls__void', 'display__String']).
hideScopeSetFrom(uniformMethods,
	['CompositeDemo','display__String']).
