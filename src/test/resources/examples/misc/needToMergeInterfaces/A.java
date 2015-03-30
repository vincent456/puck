
package needToMergeInterfaces;

interface IB {
	public void mb();
}

class B implements IB {
    public void mb(){}
}

interface IC {
	public void mb();
}

class C implements IC {
    public void mb(){}
}


//hideSetFrom(['p.B'], ['p.A']).
class A {

    private B b;
    private C c;

    public void ma(){
        b.mb();
        c.mb();
    }
}