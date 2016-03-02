package p;

interface I { void m();}
interface I2 { void m();}
abstract class AC implements I, I2 {}
class C extends AC{
    public void m(){}
}

class Test{

    I i = new C();
    I2 i2 = new C();
    AC ac = new C();
    C c = new C();

    void mi(){ i.m(); }
    void mi2(){ i2.m(); }
    void mac(){ ac.m(); }
    void mc(){ c.m(); }

}