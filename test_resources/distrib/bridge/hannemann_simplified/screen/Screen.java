package screen;

public abstract class Screen {
}

class WelcomeCapital extends Screen {
  void welcome() {
    printCapital("Welcome");
  }
  void printCapital(String s) {
    System.out.println(s.toUpperCase());
  }
}

class WelcomeStar extends Screen {
  void welcome() {
    printStar("Welcome");
  }
  void printStar(String s) {
    System.out.println("*** " + s + " ***");
  }
}

class InfoCapital extends Screen {
  void info() {
    printCapital("Some info");
  }
  void printCapital(String s) {
    System.out.println(s.toUpperCase());
  }
}

class InfoStar extends Screen {
  void info() {
    printStar("Some info");
  }
  void printStar(String s) {
    System.out.println("*** " + s + " ***");
  }
}
