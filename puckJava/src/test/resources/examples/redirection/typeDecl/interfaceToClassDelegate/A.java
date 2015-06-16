
package interfaceToClassDelegate;

interface I {
    void mUsed();
}

class Impl implements I {
    public void mUsed(){}
}

class Delegator {
    I d;
    void mUsed(){
        d.mUsed();
    }
}

class A {

    public static void main(String[] args){
        A a = new A();
        a.mUser(new Impl());
    }

    void mUser(I i){
        i.mUsed();
    }
}