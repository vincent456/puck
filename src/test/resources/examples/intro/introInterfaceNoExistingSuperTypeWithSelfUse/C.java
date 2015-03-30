package introInterfaceNoExistingSuperTypeWithSelfUse;

class C {

    private int f;

    public void fieldUserThatCanBeInInterface(){
        int dummy = this.f;
    }

}