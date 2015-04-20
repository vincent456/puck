package webMarket;

public abstract class Screen {
  abstract void printPage();
}

class WelcomeCellPhone extends Screen {
  void printPage() {
    cssCellPhone("Welcome");
  }
  void cssCellPhone(String s) {
    System.out.println(s.toUpperCase());
  }
}

class WelcomeComputer extends Screen {
  void printPage() {
    cssComputer("Welcome");
  }
  void cssComputer(String s) {
    System.out.println("*** " + s + " ***");
  }
}

class DetailArticleCellPhone extends Screen {
  void printPage() {
    cssCellPhone("Some info");
  }
  void cssCellPhone(String s) {
    System.out.println(s.toUpperCase());
  }
}

class DetailArticleComputer extends Screen {
  void printPage() {
    cssComputer("Some info");
  }
  void cssComputer(String s) {
    System.out.println("*** " + s + " ***");
  }
}
