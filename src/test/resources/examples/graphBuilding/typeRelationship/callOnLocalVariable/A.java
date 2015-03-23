package callOnLocalVariable;

class A {
    public void ma() {
        B b = new B();
        b.mb();
    }
}

class B {
    public void mb(){}
}