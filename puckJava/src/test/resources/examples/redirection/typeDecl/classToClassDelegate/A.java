
package classToClassDelegate;


class Delegatee {
    void mUsed(){}
}

class Delegator {
    Delegatee d;
    void mUsed(){
        d.mUsed();
    }
}

class A {

    public static void main(String[] args){
        A a = new A();
        a.mUser(new Delegatee());
    }

    void mUser(Delegatee d){
        d.mUsed();
    }
}