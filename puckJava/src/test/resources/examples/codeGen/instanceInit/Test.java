
class B {
    int value(){return 0;}
}

class A {

    //instance initializer
    {
        b=new B();
    }

    int j;

    A(){
        j = b.value();
    }

    final B b;


}
