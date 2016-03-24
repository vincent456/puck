package p;
import java.util.List;

interface I{ void m(); }

class C {
    List<I> is;

    void doAllM(){
        for(I i : is)
            i.m();
    }
}