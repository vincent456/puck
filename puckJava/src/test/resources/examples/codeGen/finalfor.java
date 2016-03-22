package p;
import java.util.List;
class C {
    void m(List<C> l){
        for(/*final*/ C c : l);
    }
}
