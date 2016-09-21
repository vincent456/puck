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

      compareWithExpectedAndGenerated(
        """package p;
          |class A {
          |    private int f;
          |    public void m(){}
          |}""",
        s => {
          import s.{graph, idOfFullName}

          val (AccessAbstraction(itc, _), g0) =
            Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
              Interface, SupertypeAbstraction).rvalue
          g0.addContains("p", itc)

        },
        """package p;
          |interface A_SupertypeAbstraction { void m(); }
          |class A implements A_SupertypeAbstraction {
          |    private int f;
          |    public void m(){}
          |}""")

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
        bs => {
          import bs.{graph, idOfFullName}

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
      val _ = new ScenarioFactory(
        """package p;
          |
          |class B {
          |
          |    private int f;
          |
          |    //do we put it in the interface ?
          |    //knowledge of subclass is considered bad smell so we will not (only b heuristic)
          |    public void fieldUserThatShouldNotBeInInterface(B b){ int dummy = b.f; }
          |
          |}"""
      ) {
        val packageP = fullName2id("p")
        val classB = fullName2id("p.B")

        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classB),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains(packageP, itc)


        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("field use via parameter of self type"){
      val _ = new ScenarioFactory(
        """package p;
          |
          |class C {
          |
          |    private int f;
          |
          |    public void fieldUserThatCanBeInInterface(){ int dummy = this.f; }
          |
          |}"""
      ) {
        val packageP = fullName2id("p")
        val classC = fullName2id("p.C")


        val (AccessAbstraction(itc, _), g0) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode(classC),
            Interface, SupertypeAbstraction).rvalue
        val g = g0.addContains(packageP, itc)

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }
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
      val _ = new ScenarioFactory(
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
          |}"""
      ){

        val (AccessAbstraction(itc, _), g) =
          Rules.abstracter.createAbstraction(graph, graph.getConcreteNode("p.A"),
            Interface, SupertypeAbstraction).rvalue

        assert( g.isa("p.A", itc))

        assert( g.abstractions("p.A.cannotBeInInterface(A)").isEmpty)
        assert( g.abstractions("p.A.canBeInInterface(A)").size == 1)

      }
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


}
