package p;

interface I{}
class A implements I{}

class B{
    static void m() {
        I i = new A();
    }
}

