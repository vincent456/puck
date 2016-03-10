package p;


class A {
    B ma(int i, int j){ return new B(); }
}

class B {
    C mb(){ return new C(); }
}
class C {
    void mc(){ System.out.println("hola !"); }
}

public class Test{

    public void main(String[] args){
        A a = new A();
        a.ma(0, 1).mb().mc();
    }
}
