package puck.javaGraph.commutativity

import puck.TransfoRulesSpec
import org.extendj.ExtendJGraphUtils.Rules
import puck.graph.{AccessAbstraction, NamedType, SupertypeAbstraction}
import puck.javaGraph.nodeKind.Interface
/**
  * Created by Loïc Girault on 11/24/16.
  */
class DummyIntroInterfaceTestSpec
  extends TransfoRulesSpec {


  scenario("intro interface"){
    compareWithExpectedAndGenerated(
      """package p;
        |class C {
        | public void m(){}
        |}
      """,
      s => {
        import s._
        val (AccessAbstraction(absId, _), g) =
          Rules.abstracter.createAbstraction(graph,
            graph.getConcreteNode("p.C"),
            Interface,
            SupertypeAbstraction).rvalue

        Rules.rename(g.addContains("p", absId),
          absId, "I")


      },
      """package p;
        |interface I{ void m();}
        |class C implements I {
        | public void m(){}
        |}
      """)

  }

  scenario("intro interface, only one method"){
    compareWithExpectedAndGenerated(
      """package p;
        |class C {
        | public void m(){}
        | public void m2(){}
        |}
      """,
      s => {
        import s._
        val (AccessAbstraction(absId, _), g) =
          Rules.abstracter.
            abstractTypeDeclAndReplaceByAbstractionWherePossible(
              graph, graph.getConcreteNode("p.C"), Interface,
              SupertypeAbstraction, List(graph.getConcreteNode("p.C.m()"))).rvalue

        Rules.rename(g.addContains("p", absId),
          absId, "I")

      },
      """package p;
        |interface I{ void m();}
        |class C implements I {
        | public void m(){}
        | public void m2(){}
        |}
      """)
  }

  scenario("remove interface"){
    compareWithExpectedAndGenerated(
      """package p;
        |interface I{ void m();}
        |class C implements I {
        | public void m(){}
        | public void m2(){}
        |}
      """,
      s => {
        import s.{graph, idOfFullName}

        val g2 = graph.removeIsa(NamedType("p.C"), NamedType("p.I"))
        Rules.remove.concreteNode(g2, g2.getConcreteNode("p.I")).rvalue

      },
      """package p;
        |class C {
        | public void m(){}
        | public void m2(){}
        |}
      """)

  }


  scenario("intro constructor by copy"){
    compareWithExpectedAndGenerated(
      """package p;
        |class C {
        | public C(){}
        |}
      """,
      s => {
        import s.{graph, idOfFullName}
        val (_, g) = Rules.intro.constructorByCopy(graph, "p.C").rvalue
        g
      },
      """package p;
        |class C {
        | public C(){}
        | public C(C c){}
        |}
      """)

  }
}
