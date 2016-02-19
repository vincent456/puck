package p;

import java.util.Enumeration;
import java.util.Vector;

class A {
    void m(Vector<Object> v){
        for(Enumeration<?> e = v.elements(); e.hasMoreElements();){
            Object o = e.nextElement();
        }
    }
}
