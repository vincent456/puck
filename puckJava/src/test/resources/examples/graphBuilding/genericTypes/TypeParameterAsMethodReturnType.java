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

    void doM(){
        wa.get().m();
    }
}