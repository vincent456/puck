package p;

class B {
	public void mb(){}
}
//hideScopeSetFrom(['p.B'], ['p.A']).
class A {

	public void ma(B b){
		b.mb();
	}
}