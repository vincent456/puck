package p;

//interface I {}
//class C implements I {}
//class MapOver {
//    public <S extends I> void run(S l) {}
//}
//
//public class Test {
//
//    public static void m() {
//        new MapOver().run(new C());
//    }
//}


interface I<T> {}
class C<T> implements I<T> {}

class MapOver<R> {
         public <S extends I<R>> void run(S l) {}
}

public class Test {

    public void m() {
        new MapOver<String>().run(new C<String>());
   }
}
