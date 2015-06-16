package chainedCall;

class A {
    public void ma() {
        B b = new B();
        b.mb().mc();
    }
}

class B {
    public C mb(){ return new C(); }
}

class C {
    public void mc(){}
}