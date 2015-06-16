package typeRelationship;

import java.util.List;
import java.util.ArrayList;

class A{
    void m(){}
}

class B {
    List<A> la = new ArrayList<A>();

    void mUser(){

        la.add(new A());

        la.get(0).m();
    }
}