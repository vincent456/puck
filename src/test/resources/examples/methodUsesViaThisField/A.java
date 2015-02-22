package examples.methodUsesViaThisField;

class B {
    public void mb(){}
}
//hideSetFrom(['p.B'], ['p.A']).
class A {

    private B b;

    public void ma(){
        b.mb();
    }
}