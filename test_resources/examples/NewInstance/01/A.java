package p;

class B {

	public void mb(){}
}

class A {

	public void ma(){
		B b = new B();
		b.mb();
	}
}