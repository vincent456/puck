java_import(['adapter.candidate']).

declareSet(legacy_code, ['LegacyLine', 'LegacyRectangle']).
declareSet(adapter_, ['AdapterDemo.drawLine__LegacyLine_int_int_int_int',
		'AdapterDemo.drawRectangle__LegacyRectangle_int_int_int_int']).

declareSet(client, ['AdapterDemo']).

hideScopeSetFrom(legacy_code, client).
hideSetFrom(adapter_, client).
