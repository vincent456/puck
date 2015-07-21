package p;

interface B {
    void m1();
    void m2();
}

class Bimpl implements B {
    public void m1(){}
    public void m2(){}
}

class A {

    void m(){
        Bimpl b = new Bimpl();
        b.m1();
        b.m2();
    }
}