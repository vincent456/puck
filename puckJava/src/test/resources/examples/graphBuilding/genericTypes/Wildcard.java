package p;

class A {}

class B<T>{}

class C {
    B<? super A> lowerBounded;
    B<? extends A> upperBounded;

}
