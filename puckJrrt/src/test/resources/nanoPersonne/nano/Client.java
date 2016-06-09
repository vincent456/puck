package nano;

public class Client {
  public static String client(Personne p) {
    return p.getNom();
  }
  public static void main(String[] args) {
    System.out.println(client(new Personne("titi")));
  }
}
