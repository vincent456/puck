/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.javaGraph.commutativity.move

import puck.Settings._
import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, Move}
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.{Class, Field}
import puck.TransfoRulesSpec

class MoveMethodSpec extends TransfoRulesSpec {

  feature("Move one method not used by siblings") {

    scenario("moved method not used by this") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void m(){} }
          |
          |class B { }
          |
          |class C { void user(A a){ a.m(); } }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m()"), "p.B", Some(CreateParameter)).rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class B { void m(){} }
          |
          |class C { void user(B b, A a){ b.m(); } }"""
      )
    }

    scenario("move to class of a parameter - moved is not a uses' source ") {

      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void m(B b){} }
          |
          |class B { }
          |
          |class C {
          |   A a; B b;
          |   void user(){ a.m(b); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m(B)"), "p.B").rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class B { void m(){} }
          |
          |class C {
          |   A a; B b;
          |   void user(){ b.m(); }
          |}"""
      )
    }

    scenario("move to class of a parameter - moved is not a uses' source - old receiver used as plain ref") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void m(B b){} }
          |
          |class B { }
          |
          |class C {
          |   A a; B b;
          |   void user(){ a.m(b); A a2 = a; }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m(B)"), "p.B").rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class B { void m(){} }
          |
          |class C {
          |   A a; B b;
          |   void user(){ b.m(); A a2 = a; }
          |}"""
      )
    }


    scenario("move to class of a parameter - moved uses future sibling") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void m(B b){ b.mb(); } }
          |
          |class B { void mb(){} }
          |
          |class C {
          |   A a; B b;
          |   void user(){ a.m(b); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m(B)"), "p.B").rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class B { void m(){ mb(); } void mb(){} }
          |
          |class C {
          |   A a; B b;
          |   void user(){ b.m(); }
          |}"""
      )
    }


    scenario("move to class of a parameter - moved uses old sibling") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void ma(){}  void m(B b){ ma(); } }
          |
          |class B { }
          |
          |class C {
          |   A a; B b;
          |   void user(){ a.m(b); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph.mileStone, List("p.A.m(B)"), "p.B").rvalue
          //          val ltg = Move.typeMember(graph.mileStone, List("p.A.m(B)"), "p.B")
          //          val g  = ltg.rvalue
          //          import puck.graph.transformations.Recording.RecordingOps
          //          import puck.graph.ShowDG._
          //          g

        },
        """package p;
          |
          |class A { void ma(){} }
          |
          |class B { void m(A a){ a.ma(); }  }
          |
          |class C {
          |   A a; B b;
          |   void user(){ b.m(a); }
          |}"""
      )
    }

    scenario("move to class of a parameter in another package - moved uses old sibling") {
      compareWithExpectedAndGenerated(
        Seq("""package p1;
              |import p2.B;
              |
              |public class A {
              |   public void ma(B b, int i){}
              |   public void ma(B b){ ma(b, 42); }
              |}
              |""",
          """package p2;
            |
            |public class B { }
            |"""),
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph.mileStone, List("p1.A.ma(B)"), "p2.B").rvalue
        },
        Seq("""package p1;
              |import p2.B;
              |
              |public class A {
              |   public void ma(B b, int i){}
              |
              |}
              |""",
          """package p2;
            |import p1.A;
            |
            |public class B { public void ma(A a){ a.ma(this, 42); } }
            |""")
      )
    }

    scenario("move to class of a parameter - moved uses both old and future sibling") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void ma(){}  void m(B b){ ma(); b.mb();} }
          |
          |class B { void mb(){} }
          |
          |class C {
          |   A a; B b;
          |   void user(){ a.m(b); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m(B)"), "p.B").rvalue
        },
        """package p;
          |
          |class A { void ma(){} }
          |
          |class B { void mb(){} void m(A a){ a.ma(); mb();}  }
          |
          |class C {
          |   A a; B b;
          |   void user(){ b.m(a); }
          |}"""
      )
    }

    scenario("move to class of a parameter (more than one acceptable parameter) - moved uses both old and future sibling") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A { void ma(){}  void m(B b, B b2){ ma(); b.mb(); b2.mb();} }
          |
          |class B { void mb(){} }
          |
          |class C {
          |   A a; B b;
          |   void user(){ a.m(b); }
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m(B)"), "p.B", Some(CreateParameter)).rvalue
        },
        """package p;
          |
          |class A { void ma(){} }
          |
          |class B { void mb(){} void m(A a, B b2){ a.ma(); mb(); b2.mb();}  }
          |
          |class C {
          |   A a; B b;
          |   void user(){ b.m(a); }
          |}"""
      )
    }

  }


  feature("Move one method used by siblings"){
    scenario("move method used by this - keep reference with parameter"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |
          |    public void mUser(){ m(); }
          |
          |    public void m(){}
          |}
          |
          |class B{ }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m()"), "p.B",
            Some(CreateParameter)).rvalue
        },
        """package p;
          |
          |class A {
          |    public void mUser(B b){ b.m(); }
          |}
          |
          |class B{ public void m(){} }"""
      )
    }

    scenario("move method used by this - keep reference with Field"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |
          |    public void mUser(){ m(); }
          |
          |    public void m(){}
          |}
          |
          |class B{ }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m()"), "p.B",
            Some(CreateTypeMember(Field))).rvalue
        },
        """package p;
          |
          |class A {
          |    public B b = new B();
          |    public void mUser(){ b.m(); }
          |}
          |
          |class B{ public void m(){} }"""
      )
    }

    scenario("move one of two mutually recursive methods - keep reference with Field"){
      val _ = new ScenarioFactory(
        """package example;
          |
          |class PingPong {
          |
          |    void ping(int i){
          |        if(i > 0){
          |            System.out.println("ping");
          |            pong(i - 1);
          |        }
          |    }
          |    void pong(int i){
          |        if(i > 0){
          |            System.out.println("pong");
          |            ping(i - 1);
          |        }
          |    }
          |
          |}"""
      ){

        val (pong, g) = Rules.intro(graph, "Pong", Class)
        val g1 = g.addContains("example", pong.id)

        val g2 = Move.typeMember(g1, List[NodeId]("example.PingPong.pong(int)"), pong.id,
          //Some(CreateParameter)).rvalue
          Some(CreateTypeMember(Field))).rvalue

        val recompiledEx = applyChangeAndMakeExample(g2, outDir)

        assert( Mapping.equals(g2, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - user also via self another method that will not be moved "){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |    void mUser(){
          |        m1();
          |        m2();
          |    }
          |
          |    void m1(){}
          |    void m2(){}
          |}
          |
          |class B{ }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List("p.A.m1()"), "p.B",
            Some(CreateTypeMember(Field))).rvalue
        },
        """package p;
          |
          |class A {
          |   B b = new B();
          |    void mUser(){
          |        b.m1();
          |        m2();
          |    }
          |
          |    void m2(){}
          |}
          |
          |class B{ void m1(){} }"""
      )
    }

    scenario("move method used by this several times - keep reference with Parameter"){


      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser1(){ methodToMove(); }
          |
          |    public void mUser2(){ methodToMove(); }
          |
          |    public void methodToMove(){}
          |}
          |
          |class B{ }"""
      ){

        val g = Move.typeMember(graph, List("p.A.methodToMove()"), "p.B", Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this several times - keep reference with Field"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public void mUser1(){ methodToMove(); }
          |
          |    public void mUser2(){ methodToMove(); }
          |
          |    public void methodToMove(){}
          |}
          |
          |class B{ }"""
      ){

        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g =
          Move.typeMember(graph, List(methToMove), newHostClass,
            Some(CreateTypeMember(Field))).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("moved method uses this - keep reference with parameter "){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class A {
          |
          |    public A(){}
          |
          |    public void mUsed(){}
          |
          |    public void methodToMove(){ mUsed();}
          |}
          |
          |class B{ }"""
      ){

        val g = Move.typeMember(graph, List[NodeId]("p.A.methodToMove()"), "p.B", Some(CreateParameter)).rvalue

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }
  }

  feature("Move several methods"){

    scenario("one of the moved method is used by another") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |    void m1(){ m2(); }
          |    void m2(){}
          |}
          |
          |class B{ }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List[NodeId]("p.A.m1()", "p.A.m2()"), "p.B").rvalue
        },
        """package p;
          |
          |class A { }
          |
          |class B {
          |    void m1(){ m2(); }
          |    void m2(){}
          |}""")

    }

    scenario("two moved method both used by an unmoved one") {
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |    public void mUser(){ m1(); m2(); }
          |    public void m1(){}
          |    public void m2(){}
          |}
          |
          |class B{ }""",
        bs => {
          import bs.{graph, idOfFullName}
          Move.typeMember(graph, List[NodeId]("p.A.m1()", "p.A.m2()"), "p.B", Some(CreateParameter)).rvalue
        },
        """package p;
          |
          |class A {
          |    public void mUser(B b){ b.m1(); b.m2(); }
          |}
          |
          |class B{
          |    public void m1(){}
          |    public void m2(){}
          |
          |}""")

    }
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
          bs.printFullNamesSortedByKey()
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
  }
}
