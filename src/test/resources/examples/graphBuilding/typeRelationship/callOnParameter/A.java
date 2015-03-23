package callOnParameter;

class B {
    public void mb(){}
}

class A {

    public void ma(B b){
        b.mb();
    }
}