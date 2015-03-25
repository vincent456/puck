package methodNotUsedByThis;

class A {
    public void ma(){}
}

class B {

}

class C {
    public void mc(){
        A a = new A();
        a.ma();
    }
}