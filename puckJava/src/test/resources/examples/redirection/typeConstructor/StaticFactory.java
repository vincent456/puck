package p;


class Factory{

}

class B {
    B(){}

    static B createB(){
        return new B();
    }
}

class A {
    void m() {
       B b = new B();
    }
}