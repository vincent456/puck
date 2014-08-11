package p;

class B {
	public void mb(){}
}
//hideScopeSetFrom(['p.B.mb__void'], ['p.A']).
class A {

	public void ma(B b){
		b.mb();
	}
}