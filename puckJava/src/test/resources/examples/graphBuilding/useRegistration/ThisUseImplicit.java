package p;

class A {

    void m(){}

    void mUserViaThis(){
        m();
    }

    void mUserViaParameter(A a){
        a.m();
    }

}