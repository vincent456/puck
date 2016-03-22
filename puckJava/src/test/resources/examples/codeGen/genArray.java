package p;

interface Set<E> {
    <T> T[] toArray(T[] a);
}

public class Test {

    public void m(Set<Long> ns) {
        Long[] a = ns.toArray(new Long[0]);
    }
}


//interface Set {
//    <T> T[] toArray(T[] a);
//}
//
//public class Test {
//
//    public void m(Set ns) {
//        Long[] a = ns.toArray(new Long[0]);
//    }
//}