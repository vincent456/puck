package p;

class A {
	public void m(A a){}
}
class B {
	public void mi(A a1, A a2){
		a1.m(a2);
	}
}
//hideScopeSetFrom(['A'], ['B']).
//if we make a "simple" extract interface of A 
// and replace its uses we obtain the following

/*interface AI {
	void m(A a);
}

class A implements AI{

	int afield

	void m(A a){
		//a.afield // then what ??
		a.m(this)
	}
}
class B {

	void mi(AI a1, AI a2){
		a1.m(a2);
	}
}*/

//then the code won't compile since m takes an A parameter
//Expected result : 

/*interface AI {
	void m(AI a);
}

class A implements AI{
	void m(AI a){}
}
class B {

	void mi(AI a1, AI a2){
		a1.m(a2);
	}
}*/

