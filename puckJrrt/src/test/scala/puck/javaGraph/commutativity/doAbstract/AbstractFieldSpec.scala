package puck.javaGraph.commutativity.doAbstract

import puck.TransfoRulesSpec
import puck.graph.{AccessAbstraction, DelegationAbstraction, ReadWriteAbstraction}
import puck.jastadd.ExtendJGraphUtils._
import puck.javaGraph.nodeKind.{Method, StaticMethod}

/**
  * Created by Loïc Girault on 26/08/16.
  */
class AbstractFieldSpec extends TransfoRulesSpec {


  scenario("field not used abstracted") {
    compareWithExpectedAndGenerated(
      """package p;
        |class A { int f; }
      """,
      bs => {
        import bs.{graph, idOfFullName}

        val (_,g2) =  Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A.f",
            Method, DelegationAbstraction).rvalue

//        import puck.graph.ShowDG._
//        (g2, g2.edges).println
        g2
      },
      """package p;
        |class A{
        |     int f;
        |      public int getF(){ return f; }
        |      public int setF(int f){
        |         this.f = f;
        |         return this.f;
        |      }
        | }
      """)
  }
}
