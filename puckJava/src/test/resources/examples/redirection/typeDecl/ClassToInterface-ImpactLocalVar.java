package p;

class Wrapper {
    A a = new A();
    public A get(){return a;}

}

interface I {  }

class A implements I {  }


class B {
    Wrapper wa = new Wrapper();

    void getA(){ A a = wa.get(); }
}