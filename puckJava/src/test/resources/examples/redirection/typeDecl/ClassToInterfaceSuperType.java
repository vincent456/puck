package p;

class ClassUsed implements SuperType{
    public void mUsed(){}
}

interface SuperType {
    void mUsed();
}

class A {

    public static void main(String[] args){
        A a = new A();
        a.mUser(new ClassUsed());
    }

    void mUser(ClassUsed cu){
        cu.mUsed();
    }
}