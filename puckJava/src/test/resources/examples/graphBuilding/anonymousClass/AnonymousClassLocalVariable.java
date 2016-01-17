package p;

interface DoM {
    void m();
}

class A {

    void ma(){
        DoM d2 = new DoM(){
            public void m(){
                System.out.println("also do m !");
            }
        };
    }
}