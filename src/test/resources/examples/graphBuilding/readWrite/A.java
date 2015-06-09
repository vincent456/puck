package p;

class A {
    int f = 0;

    public int getF(){
        return f;
    }

    public void setF(int f){
        this.f = f;
    }

    public void incF(){
        //f++; //not handled !
        this.f = f + 1;
    }

}