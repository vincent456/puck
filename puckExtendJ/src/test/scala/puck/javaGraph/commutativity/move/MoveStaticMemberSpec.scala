package puck.javaGraph.commutativity.move

import puck.TransfoRulesSpec
import puck.graph.Factory
import puck.graph.transformations.rules.Move

/**
  * Created by Loïc Girault on 30/08/16.
  */
class MoveStaticMemberSpec
  extends TransfoRulesSpec  {

  scenario("Move field between classes in same package") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class A { static int i = 42; }
        |
        |class B {
        |    public static void m(){
        |         int j = A.i;
        |    }
        |}
        |
        |class C { } """,
      s => {
        import s.{graph, idOfFullName}
        Move.staticDecl(graph, "p.A.i", "p.C").rvalue
      },
      """package p;
        |
        |class A {  }
        |
        |class B {
        |    public static void m(){
        |         int j = C.i;
        |    }
        |}
        |
        |class C { static int i = 42; } """
    )
  }

  feature("Move static method") {
    scenario("unused factory") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {  static A createA(){return new A();} }
          |
          |class Client {
          |    void m(){ A a = new A(); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          val g = graph.setRole("p.A.createA()", Some(Factory("p.A.A()")))
          Move.staticDecl(g, "p.A.createA()", "p.Client").rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class Client {
          |    static A createA(){return new A();}
          |    void m(){ A a = new A(); }
          |}""")
    }

    scenario("used factory moved in client") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {  static A createA(){return new A();}  }
          |
          |
          |class Client { void m(){ A a = A.createA(); } }""",
        bs => {
          import bs.{graph, idOfFullName}
          val g = graph.setRole("p.A.createA()", Some(Factory("p.A.A()")))
          Move.staticDecl(g, "p.A.createA()", "p.Client").rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class Client {
          |    static A createA(){return new A();}
          |    void m(){ A a = createA(); }
          |}""")


    }

    scenario("used factory moved in outsider host") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { static A createA(){return new A();} }
          |
          |class Factory{ }
          |
          |class Client { void m(){ A a = A.createA(); } }""",
        bs => {
          import bs.{graph, idOfFullName}
          val g = graph.setRole("p.A.createA()", Some(Factory("p.A.A()")))
          Move.staticDecl(g, "p.A.createA()", "p.Factory").rvalue
        },
        """package p;
          |
          |class A {  }
          |
          |class Factory{ static A createA(){return new A();} }
          |
          |class Client { void m(){ A a = Factory.createA(); } }""")

    }

    scenario("static method using another (not-moved) static method") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |   static int get42(){ return times2(21); }
          |   static int times2(int i){ return i * 2; }
          |}
          |
          |class B{ }
          |
          |class C{ void m(){ int i = A.get42(); } }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.staticDecl(graph, "p.A.get42()", "p.B").rvalue
        },
        """package p;
          |
          |class A { static int times2(int i){ return i * 2; } }
          |
          |class B { static int get42(){ return A.times2(21); } }
          |
          |class C { void m(){ int i = B.get42(); } }"""
      )
    }

    scenario("static method using another (not-moved) static method - used as constructor arg") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |   static int get42(){ return times2(21); }
          |   static int times2(int i){ return i * 2; }
          |}
          |
          |class B{ }
          |
          |class D { D(int i){} }
          |
          |class C{ void m(){ D d = new D(A.get42()); } }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.staticDecl(graph, "p.A.get42()", "p.B").rvalue
        },
        """package p;
          |
          |class A { static int times2(int i){ return i * 2; } }
          |
          |class B { static int get42(){ return A.times2(21); } }
          |
          |class D { D(int i){} }
          |
          |class C { void m(){ D d = new D(B.get42()); } }"""
      )
    }

    scenario("static method using another (not-moved) static method - different packages") {
      compareWithExpectedAndGenerated(
        Seq(
          """package p1;
            |
            |public class A {
            |   public static int get42(){ return times2(21); }
            |   public static int times2(int i){ return i * 2; }
            |}""",
          """package p2;
            |public class B{ } """,
          """package p3;
            |import p1.A;
            |public class C{ void m(){ int i = A.get42(); } }"""),
        bs => {
          import bs.{graph, idOfFullName}
          Move.staticDecl(graph, "p1.A.get42()", "p2.B").rvalue
        },
        Seq(
          """package p1;
            |public class A {
            |   public static int times2(int i){ return i * 2; }
            |}""",
          """package p2;
            |import p1.A;
            |public class B{ public static int get42(){ return A.times2(21); } } """,
          """package p3;
            |import p2.B;
            |public class C{ void m(){ int i = B.get42(); } }""")
      )

    }

    scenario("static method using another (not-moved) static method - used as constructor arg, different packages") {
      compareWithExpectedAndGenerated(
        Seq(
          """package p1;
            |
            |public class A {
            |   public static int get42(){ return times2(21); }
            |   public static int times2(int i){ return i * 2; }
            |}""",
          """package p2;
            |public class B{ } """,
          """package p3;
            |import p1.A;
            |class D { D(int i){} }
            |class C { void m(){ D d = new D(A.get42()); } }"""),
        bs => {
          import bs.{graph, idOfFullName}
          Move.staticDecl(graph, "p1.A.get42()", "p2.B").rvalue
        },
        Seq(
          """package p1;
            |public class A {
            |   public static int times2(int i){ return i * 2; }
            |}""",
          """package p2;
            |import p1.A;
            |public class B{ public static int get42(){ return A.times2(21); } } """,
          """package p3;
            |import p2.B;
            |class D { D(int i){} }
            |class C { void m(){ D d = new D(B.get42()); } }""")
      )

    }
    scenario("static method using a sibling (not-moved) static field -  different packages") {
      compareWithExpectedAndGenerated(
        Seq(
          """package p1;
            |
            |public class A {
            |   private static int i42 = 42;
            |   public static int get42(){ return i42; }
            |}""",
          """package p2;
            |public class B{ } """,
          """package p3;
            |import p1.A;
            |class D { D(int i){} }
            |class C { void m(){ D d = new D(A.get42()); } }"""),
        bs => {
          import bs.{graph, idOfFullName}
          //bs.printFullNamesSortedByKey()
          Move.staticDecl(graph, "p1.A.get42()", "p2.B").rvalue
        },
        Seq(
          """package p1;
            |public class A {
            |   public static int i42 = 42;
            |}""",
          """package p2;
            |import p1.A;
            |public class B{ public static int get42(){ return A.i42; } } """,
          """package p3;
            |import p2.B;
            |class D { D(int i){} }
            |class C { void m(){ D d = new D(B.get42()); } }""")
      )

    }


    scenario("static method within same package used and used from another package with potential name conflict") {
      info("example seen in marauroa")
      compareWithExpectedAndGenerated(
        Seq(
          """package p;
            |
            |public class A {  public static void m(){}  }
            |public class B { }""",
          """package p2;
            |
            |import p.A;
            |
            |class C {
            |   int p;
            |   void m2(){ A.m(); }
            |}"""
        ),
        bs => {
          import bs.{graph, idOfFullName}
          Move.staticDecl(graph, "p.A.m()", "p.B").rvalue
        },
        Seq(
          """package p;
            |
            |public class A { }
            |public class B { public static void m(){} }""",
          """package p2;
            |
            |import p.B;
            |
            |class C {
            |   int p;
            |   void m2(){ B.m(); }
            |}"""
        ))


    }
  }

}
