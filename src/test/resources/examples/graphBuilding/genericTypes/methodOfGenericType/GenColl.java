package methodOfGenericType;

import java.util.List;

class A{}

class User {

    public void m(){
        GenColl<A> colla = new GenColl<A>();
        colla.put(new A());
    }

}

class GenColl<T> {
    public void put(T t){}
}