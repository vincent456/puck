package p;

class A {
    A(){}

    static A createA(){return new A();}

}

class Factory{

}

class Client {
    void m(){
        A a = A.createA();
    }
}