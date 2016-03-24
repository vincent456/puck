package p;

class Wrapper<T> {
    private T t;
    public void set(T t){}
    public T get(){return t;}

}

interface I { void m(); }

class A implements I { public void m(){} }


class B {
    Wrapper<A> wa = new Wrapper<A>();

    void doM(){ wa.get().m(); }
}