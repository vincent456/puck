

class A {
	A fieldA;


	void n(){}

	A mi(){
		return new A();
	}

	void mj(A para){
		para.n();
	}

	A mk(A para){
		
		para.n();

		return para.mi();
	}
}

class B {
	A fieldA;
}

/* 
 hideScopeFrom(A, B).
 expected result : 
*/

/*
interface AI {
	void n();
	void mj(AI para);

	AI mi();
	AI mk(AI para);
}

class A {
	AI fieldA;


	void n(){}

	A mi(){
		return new A();
	}

	void mj(A para){
		para.n();
	}

	//whereas we can keep the return type A in mi (because of covariance of codomain),
	//here we must generalize the return type because the method is called on a generalized parameter
	AI mk(AI para){
		para.n();
		return para.mi();
	}
}

class B {
	AI fieldA;
}*/
