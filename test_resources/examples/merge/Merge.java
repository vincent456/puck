
package p;

class C {}

interface I {
    C giveC();
    void usesI(I i);
}

class A implements I {

    public C giveC(){
        return new C();
    }
    public void usesI(I i){
        i.giveC();
    }

}

interface I2 {
    C giveC();
    void usesI(I2 i);
}

class B implements I2 {

    public C giveC(){
        return new C();
    }
    public void usesI(I2 i){
        i.giveC();
    }

}