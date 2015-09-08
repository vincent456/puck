package p;

class F{}

public class A {

    private F f; //not initialized means not initialized outside of the constructor
    public A(){
        f = new F();
    }

}