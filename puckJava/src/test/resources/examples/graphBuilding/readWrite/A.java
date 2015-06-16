package p;

class A {
    int f = 0;

    public int getF() {
        return f;
    }

    public void setF(int f) {
        this.f = f;
    }

    public void incF0() {
        //f++; //not handled !
        this.f = f + 1;
    }

    public void incF1() {
        f++;
    }

    public void incF2() {
        ++f;
    }

    public void decF0() {
        --f;
    }
    public void decF1() {
        f--;
    }
}
