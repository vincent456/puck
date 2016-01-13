package p;

class A {

    int x;

    A(int i){ x = i;}

}

class B extends A{

    int y;

    B(int i, int j){
        super(i);
        y = j;
    }

}