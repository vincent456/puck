package p;

class A {

    void m(){
        System.out.println("A.m()");
    }
}
class Cond {

    //typeMember use
    // Cond.condM(boolean, A).definition uses A.m()
    //Types Uses
    //Cond.condM.a uses A
    //A.A() uses A
    void condM(boolean c, A a) {
        (c ? new A() : a).m();
    }
}

//class A {
//    void m(){System.out.println("A.m()");}
//}
//
//class B extends I {
//    void m(){System.out.println("B.m()");}
//}
//
//class Cond {
//
//    void m(boolean c) {
//        (c ? new A() : new B()).m();
//    }
//}