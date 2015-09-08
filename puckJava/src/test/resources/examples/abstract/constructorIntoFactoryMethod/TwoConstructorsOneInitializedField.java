package p;

class F{}
class G{}
public class A {

    private F f = new F();
    private G g;
    public A() {
        g = new G();
    }
    public A(G g){
        this.g = g;
    }
}