package p;

class A {

    private int f;

    public void m(int i){}

    public void canBeInInterface(A a1){
        A a2 = new A();
        a1.m(a2.f);
    }

    public void cannotBeInInterface(A a1){
        A a2 = new A();
        a2.m(a1.f);
    }
}