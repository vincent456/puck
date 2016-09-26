package puck.javaGraph.commutativity.doAbstract

import puck.Settings._
import puck.TransfoRulesSpec
import puck.graph.{AccessAbstraction, SupertypeAbstraction}
import puck.graph.comparison.Mapping
import puck.jastadd.ExtendJGraphUtils._
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Interface

/**
  * Created by Loïc Girault on 26/08/16.
  */
class AbstractTypeSpec extends TransfoRulesSpec {


  feature("Abstract class into interface") {

    info("no pre-existing super type")

    scenario("simple case - method without args") {
      def code( interface : String ="", implementsClause : String="") : String =
        s"""package p;
          |$interface
          |class A $implementsClause {
          |    private int f;
          |    public void m(){}
          |}"""

      compareWithExpectedAndGenerated(code(),
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)

        }, code(
          "interface A_SupertypeAbstraction { void m(); }",
          "implements A_SupertypeAbstraction"))

    }

    scenario("simple case - method with one arg"){
      compareWithExpectedAndGenerated(
        """package p;
          |class B {}
          |class A { public void m(B b){} }""",
        bs => {
          import bs.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |class B {}
          |interface A_SupertypeAbstraction { public void m(B b); }
          |class A implements A_SupertypeAbstraction { public void m(B b){} } """)

    }

    scenario("method return wildcard param type"){

      compareWithExpectedAndGenerated(
        """package p;
          |import java.util.Enumeration;
          |
          |class C { public Enumeration<?> getThem(){ return null; }  }""",
        bs => {
          import bs.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.C"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |import java.util.Enumeration;
          |
          |interface C_SupertypeAbstraction { Enumeration<?> getThem(); }
          |class C implements C_SupertypeAbstraction { public Enumeration<?> getThem(){ return null; }  }""")

    }

    scenario("method self use in class"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |    public void m(){}
          |    public void methodUser(A a){ a.m(); }
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue

          g0.addContains("p", itc)
        },
        """package p;
          |
          |interface A_SupertypeAbstraction {
          |    void m();
          |    void methodUser(A_SupertypeAbstraction a);
          |}
          |class A implements A_SupertypeAbstraction{
          |    public void m(){}
          |    public void methodUser(A_SupertypeAbstraction a){ a.m(); }
          |}""")
    }

    scenario("field self use in class"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class B {
          |
          |    private int f;
          |
          |    //do we put it in the interface ?
          |    //knowledge of subclass is considered bad smell so we will not (only b heuristic)
          |    public void m(B b){ int dummy = b.f; }
          |
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.B"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |interface B_SupertypeAbstraction {}
          |
          |class B implements B_SupertypeAbstraction{
          |
          |    private int f;
          |
          |    public void m(B b){ int dummy = b.f; }
          |
          |}""")
    }

    scenario("field use via parameter of self type"){

      compareWithExpectedAndGenerated(
        """package p;
          |
          |class B {
          |
          |    private int f;
          |
          |    public void m(){ int dummy = this.f; }
          |
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.B"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |interface B_SupertypeAbstraction { void m(); }
          |
          |class B implements B_SupertypeAbstraction{
          |
          |    private int f;
          |
          |    public void m(){ int dummy = this.f; }
          |
          |}""")
    }




    scenario("use of type member sibling by self and parameter"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A a){ a.m(this.f); }
          |
          |    public void cannotBeInInterface(A a){ this.m(a.f); }
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A",
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |
          |interface A_SupertypeAbstraction {
          |    void m(int i);
          |    void canBeInInterface(A_SupertypeAbstraction a);
          |}
          |
          |class A implements A_SupertypeAbstraction{
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A_SupertypeAbstraction a){ a.m(this.f); }
          |
          |    public void cannotBeInInterface(A a){ this.m(a.f); }
          |}"""
      )
    }

    ignore("use of type member sibling by local variable and parameter"){

      compareWithExpectedAndGenerated(
        """package p;
          |
          |class A {
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A a1){
          |        A a2 = new A();
          |        a1.m(a2.f);
          |    }
          |
          |    public void cannotBeInInterface(A a1){
          |        A a2 = new A();
          |        a2.m(a1.f);
          |    }
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A",
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |
          |interface A_SupertypeAbstraction {
          |    void m(int i);
          |    void canBeInInterface(A_SupertypeAbstraction a);
          |}
          |
          |class A implements A_SupertypeAbstraction {
          |
          |    private int f;
          |
          |    public void m(int i){}
          |
          |    public void canBeInInterface(A_SupertypeAbstraction a1){
          |        A a2 = new A();
          |        a1.m(a2.f);
          |    }
          |
          |    public void cannotBeInInterface(A a1){
          |        A a2 = new A();
          |        a2.m(a1.f);
          |    }
          |}"""
      )
    }

    info("super type already present")
    scenario("existing supertype - simple case"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |interface SuperA {
          |    void m1();
          |}
          |
          |class A implements SuperA {
          |    public void m1(){}
          |    public void m2(){}
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue

          g0.addContains("p", itc)
        },
        """package p;
          |
          |interface SuperA {
          |    void m1();
          |}
          |interface A_SupertypeAbstraction extends SuperA {
          |    void m1();
          |    void m2();
          |}
          |class A implements A_SupertypeAbstraction {
          |    public void m1(){}
          |    public void m2(){}
          |}"""
      )
    }

    scenario("method throw exception"){

      compareWithExpectedAndGenerated(
        """package p;
          |import java.io.IOException;
          |
          |class C { public void m() throws IOException { }  }""",
        bs => {
          import bs.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.C"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)
        },
        """package p;
          |import java.io.IOException;
          |
          |interface C_SupertypeAbstraction { void m() throws IOException; }
          |class C implements C_SupertypeAbstraction { public void m() throws IOException {  }  }""")

    }
  }

  feature("Abstract gen class into gen interface") {
    scenario("simple case") {

      def code( interface : String ="", implementsClause : String="") : String =
        s"""package p;
          |$interface
          |class A<T> $implementsClause {
          |    private T f;
          |    public void m(){}
          |}"""


      compareWithExpectedAndGenerated(code(),
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)

        },code(
          "interface A_SupertypeAbstraction<T> { public void m(); }",
          "implements A_SupertypeAbstraction<T>"
        ))
    }

    scenario("method return typed with type parameter") {

      def code( interface : String ="", implementsClause : String="") : String =
        s"""package p;
            |$interface
            |class A<T> $implementsClause {
            |    private T f;
            |    public T m(){ return f;}
            |}"""


      compareWithExpectedAndGenerated(code(),
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)

        },code(
          "interface A_SupertypeAbstraction<T> { public T m(); }",
          "implements A_SupertypeAbstraction<T>"
        ))
    }
  }
}
