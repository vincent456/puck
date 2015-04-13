package thisUseExplicit;

class A {

    void m(){}

    void mUserViaThis(){
        this.m();
    }

    void mUserViaParameter(A a){
        a.m();
    }

}