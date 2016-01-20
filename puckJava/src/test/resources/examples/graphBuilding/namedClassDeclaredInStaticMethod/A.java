package p;

interface CanDoM{
    void m();
}

public class A {

    public static void declareInnerClass(){
        class CanDoMInstance implements CanDoM {
            public void m(){
                System.out.println("Because it compiles !");
            }
        }
        (new CanDoMInstance()).m();
    }





}