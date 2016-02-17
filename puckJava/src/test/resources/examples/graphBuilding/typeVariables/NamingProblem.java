package p;


//interface Function<S,T>{}
interface Comparator<T> {

        //real exemple comming from java.util.Comparator
//        <U> Comparator<T> thenComparing(Function<? super T, ? extends U> keyExtractor,
//                                        Comparator<? super U> keyComparator);
        <U> Comparator<T> thenComparing(Comparator<? super U> keyComparator);
}


