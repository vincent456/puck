
java_import(['screen']).

declareSet(implementations, 
		['WelcomeCapital.printCapital__String', 
		 'WelcomeStar.printStar__String',
		 'InfoCapital.printCapital__String', 
		 'InfoStar.printStar__String']).

declareSet(abstractions, ['screen', 'Screen', 
						'WelcomeCapital', 'WelcomeStar',
						'InfoCapital', 'InfoStar']).

hideSetFrom(implementations, abstractions).

