package p;

class A {
    public void ma(B b){b.mb();}
}

class B {
    public void mb(){}
}

class C {
    public void mc(){
        A a = new A();
        a.ma(new B());
    }
}