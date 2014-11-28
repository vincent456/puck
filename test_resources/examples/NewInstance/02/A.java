package p;
//hideScopeSetFrom(['p.B'], ['p.A']).
class B {

	public void mb(){}
}

class C {
	public B getB(){
		return new B();
	}
}

class A {
	C c = new C();

	public void ma(){
		B b = c.getB();
		b.mb();
	}
}