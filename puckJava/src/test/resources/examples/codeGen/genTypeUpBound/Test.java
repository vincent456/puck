
class A{}

interface I0<X>{
    X get();
}

interface I {
    I0<? extends A> m();
}

class B{
    void mtest(I i){
        I0<? extends A> i0 = i.m();
        A a = i0.get();
    }
}
