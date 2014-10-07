package p;
class A {

	void mi();

	void mj()
}
//manque de precision du graphe
//hideScopeFrom('A.mi', 'B')
//ou
//hideScopeFrom('A', 'C')

class B {

	void ml(A a){
		A a2 = new A();

		a2.mi();

		a.mj();
	}

}

class C {
	void mn(A a){
		//..

		b.ml(a);

		//..
	}
}