package p;

import java.util.Collection;
import java.util.Vector;

class C<T> {
    public C(Collection<T> c){}
}

public class Test{
    public void main(String[] arg){
        Vector<String> v = new Vector<String>();
        C<String> c = new C<String>(v);
    }
}
