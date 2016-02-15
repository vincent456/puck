package p;

class A<T>{

    <T> T castMe(Object o){
        return (T)o;
    }

    <T> T castMeInstead(Object o){
        return (T)o;
    }
}