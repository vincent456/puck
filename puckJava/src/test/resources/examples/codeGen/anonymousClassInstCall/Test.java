package p;

interface A{ void ma();}

public class Test{
    public void main(String[] args){
        new A(){
           public void ma(){System.out.println("I'm an anonymous A !");}
        }.ma();
    }
}
