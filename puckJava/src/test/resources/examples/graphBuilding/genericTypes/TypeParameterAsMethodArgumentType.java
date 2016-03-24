package p;

class Wrapper<T> {
    private T t;

    public void set(T t){}
    public T get(){return t;}

}

class A{
    void m(){}
}

class B {
    Wrapper<A> wa = new Wrapper<A>();
    A a = new A();

    void init(){
        wa.set(a);
    }
}