package p;

interface DoM {
    void m();
}

class A {

    DoM f = new DoM(){
        public void m(){
            System.out.println("do m !");
        }
    };

}