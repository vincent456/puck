package p;

class B {
    public void mb(){}
}

class A {

    private B b;

    public void ma(){
        b.mb();
    }
}