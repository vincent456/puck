package puck.javaGraph.commutativity.doAbstract

import puck.TransfoRulesSpec
import puck.graph.{AccessAbstraction, DelegationAbstraction}
import puck.jastadd.ExtendJGraphUtils._
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


}
