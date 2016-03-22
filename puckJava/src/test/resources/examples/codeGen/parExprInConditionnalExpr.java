package p;

class C {
    C(String str){}

    String getText(){return "";}

    void init(Object o){
        new C(o == null ? (getText()) : "");
    }


}