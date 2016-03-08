package p2;

import p1.A;

class B<T>{
    void m(T t){}
}

class C{

    B<A> ba;
    void m(){
        ba.m(new A());
    }
}

