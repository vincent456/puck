package p;

class C {
    C m0(){ return new C();}

    void m1(){}

    static void m(C c){
        c.m0().m1();
    }

}
