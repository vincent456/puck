package p;



interface SuperA{
    public void mInInterface();
}

class A implements SuperA {
    public void mInInterface(){}
    public void mNotInInterface(){}
}