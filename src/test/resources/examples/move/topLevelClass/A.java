package topLevelClass.p1;

class A {
    public void ma(){}
}

class B {
    public void mb(){
        A a = new A();
        a.ma();
    }
}