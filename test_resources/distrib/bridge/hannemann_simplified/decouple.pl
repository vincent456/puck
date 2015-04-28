import ['screen']

implementations =
		['WelcomeCapital.printCapital__String', 
		 'WelcomeStar.printStar__String',
		 'InfoCapital.printCapital__String', 
		 'InfoStar.printStar__String']

abstractions = ['Screen', 'WelcomeCapital', 'WelcomeStar',
			  'InfoCapital', 'InfoStar']

hide implementations from abstractions
