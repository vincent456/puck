package p;

import java.util.Collection;
import java.util.Vector;

public class Test{
    public void main(String[] arg){
        Collection<String> v1 = new Vector<String>();
        Vector<String> v2 = new Vector<String>(v1);
    }
}
