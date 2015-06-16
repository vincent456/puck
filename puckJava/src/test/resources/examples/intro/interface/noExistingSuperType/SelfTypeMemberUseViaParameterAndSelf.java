package p;

class A {

    private int f;

    public void m(int i){}

    public void canBeInInterface(A a){
        a.m(this.f);
    }

    public void cannotBeInInterface(A a){
        this.m(a.f);
    }
}