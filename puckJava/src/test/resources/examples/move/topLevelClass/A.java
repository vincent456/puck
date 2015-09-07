package p1;

public class A {
    public A(){}
    public void ma(){}
}

class B {
    public void mb(){
        A a = new A();
        a.ma();
    }
}