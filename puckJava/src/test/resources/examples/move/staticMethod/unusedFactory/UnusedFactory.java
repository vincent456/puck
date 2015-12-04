package p;

class A {
    A(){}

    static A createA(){return new A();}

}

class Client {
    void m(){
        A a = new A();
    }
}