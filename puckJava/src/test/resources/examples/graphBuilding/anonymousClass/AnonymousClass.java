package examples.graphBuilding.anonymousClass;

interface DoM {
    void m();
}

class A {

    DoM d1 = new DoM(){
        public void m(){
            System.out.println("do m !");
        }
    };


    void ma(){
        DoM d2 = new DoM(){
            public void m(){
                System.out.println("also do m !");
            }
        };
    }
}