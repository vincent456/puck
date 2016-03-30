package p;

class Wrapper<T> {
    private T t;
    public void set(T t){}
    public T get(){return t;}

}

interface I { }

class A implements I { }


class B {
    Wrapper<A> wa = new Wrapper<A>();

    void getA(){ A a = wa.get(); }
}