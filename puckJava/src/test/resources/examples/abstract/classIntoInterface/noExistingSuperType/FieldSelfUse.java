package p;

class B {

    private int f;

    //do we put it in the interface ?
    //knowledge of subclass is considered bad smell so we will not (only b heuristic)
    public void fieldUserThatShouldNotBeInInterface(B b){
        int dummy = b.f;
    }

}