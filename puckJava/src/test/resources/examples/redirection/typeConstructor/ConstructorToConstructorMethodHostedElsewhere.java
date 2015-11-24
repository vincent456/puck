package p;


class Factory{
    Factory(){}
    B createB(){
        return new B();
    }
}

class B {
    B(){}
}

class A {
    void m() {
       B b = new B();
    }
}