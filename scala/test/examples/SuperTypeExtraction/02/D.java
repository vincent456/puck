package p;

interface C{
	void m(A a);
}
//hideScopeFrom(A, D)
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

// ~ expected result : 

/*interface AI {
	void mk();
	AI create();
}
class A {
	A create {
		return new A();
	}
    void mk(){}
}


interface C{
	void m(AI a); //impacted by m1 .. but have to check all class implementing C !
}

class D {
	void m1(C c, AI aFactory){
		AI a = aFactory.create();
		//impact m signature
		c.m(a);
	}

	void m2(AI aFactory){
		AI a = aFactory.create();
		a.mk();//no outside impact
	}
	AI m3(AI aFactory){//impact mi uses of m3
		return aFactory.create();
	}
	void m4(AI a){
		//no outside impact
		a.mk();
	}
}
class DUser{
	void mi(D d){
		AI a = d.m3(new A());//impacted by m3
	}
	void mj(D d){
		A a = new A();
		d.m4(a);
	}
}*/

