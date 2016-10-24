package puck.javaGraph.commutativity.doAbstract

import puck.TransfoRulesSpec
import puck.graph.{AccessAbstraction, DelegationAbstraction}
import puck.jastadd.ExtendJGraphUtils._
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.StaticMethod

/**
  * Created by Loïc Girault on 26/08/16.
  */
class AbstractConstructorSpec extends TransfoRulesSpec {



  scenario("static factory method") {
    compareWithExpectedAndGenerated(
      """package p;
        |class FactoryClass{}
        |class A{}
      """,
      bs => {
        import bs.{graph, idOfFullName}
        val (AccessAbstraction(factoryMethod, _), g) =
          Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A.A()",
            StaticMethod, DelegationAbstraction).rvalue
        g.addContains("p.FactoryClass", factoryMethod)
      },
      """package p;
        |class FactoryClass{ static A create(){return new A();}}
        |class A{}
      """)
  }

  scenario("static factory method with one arg") {
    compareWithExpectedAndGenerated(
      """package p;
        |class FactoryClass{}
        |class A{ int f; A(int f){this.f = f;} }
      """,
      bs => {
        import bs.{graph, idOfFullName}
        val (AccessAbstraction(factoryMethod, _), g) =
          Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A.A(int)",
            StaticMethod, DelegationAbstraction).rvalue

        g.addContains("p.FactoryClass", factoryMethod)
      },
      """package p;
        |class FactoryClass{ static A create(int f){return new A(f);}}
        |class A{ int f; A(int f){this.f = f;} }
      """)
  }


  ignore("one constructor no initialized field"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class F{}
        |
        |public class A {
        |
        |    private F f; //not initialized means not initialized outside of the constructor
        |    public A(){ f = new F(); }
        |
        |}"""
    ) {

    }
  }

  ignore("one constructor one initialized field"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class F{}
        |
        |public class A {
        |
        |    private F f = new F();
        |    public A(){}
        |}"""
    ) {

    }
  }

  ignore("two constructors one initialized field"){
    val _ = new ScenarioFactory(
      """package p;
        |
        |class F{}
        |class G{}
        |public class A {
        |
        |    private F f = new F();
        |    private G g;
        |    public A() { g = new G(); }
        |    public A(G g){ this.g = g; }
        |}"""
    ) {

    }
  }

}
