package p;

interface C{
	void m(A a);
}
class A {
    public void mk(){}
}
class D {
	public void m1(C c){
		A a = new A();
		c.m(a);
	}
	public void m2(){
		A a = new A();
		a.mk();
	}
	public A m3(){
		return new A();
	}
	public void m4(A a){
		a.mk();
	}
}

class DUser{
	public void mi(D d){
		A a = d.m3();
	}
	public void mj(D d){
		A a = new A();
		d.m4(a);
	}
}
