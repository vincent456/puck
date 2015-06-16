package constructorToConstructorMethodHostedBySelf;


class B{
    B(){}

    B create(){return new B();}
}

class A {
    void m(){
        new B();
    }
}

class C {
    void mc(){
        A a = new A();
        a.m();
    }
}