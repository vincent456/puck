package puck.javaGraph.commutativity.doAbstract

import puck.TransfoRulesSpec
import puck.graph.{DelegationAbstraction, ReadWriteAbstraction}
import org.extendj.ExtendJGraphUtils._
import puck.javaGraph.nodeKind.Method

/**
  * Created by Loïc Girault on 26/08/16.
  */
class AbstractFieldSpec extends TransfoRulesSpec {


  val setter = "public int setF(int f){ this.f = f; return this.f; }"
  val getter = "public int getF(){ return f; }"

  def code(g : String ="", s : String = "") : String =
    s"""package p;
      |class A { int f;
      | $g
      | $s
      |}
    """

  scenario("field not used abstracted") {
    compareWithExpectedAndGenerated(code(),
      bs => {
        import bs.{graph, idOfFullName}
        val (_,g2) =  Rules.abstracter.createAbstraction(graph, graph getConcreteNode "p.A.f",
            Method, DelegationAbstraction).rvalue
        g2
      },code(getter, setter))
  }
  scenario("field not used with getter abstracted") {
    compareWithExpectedAndGenerated(code(getter),
      bs => {
        import bs.{graph, idOfFullName}

        val abs = graph.abstractions("p.A.f")
        assert(abs.size == 1)
        assert(abs.head == ReadWriteAbstraction(Some("p.A.getF()"), None))

        Rules.abstracter.completeReadWriteAbstraction(graph,
          graph getConcreteNode "p.A.f",
          abs.head.asInstanceOf[ReadWriteAbstraction])
      },code(getter, setter))
  }

  scenario("field not used with setter abstracted") {
    compareWithExpectedAndGenerated(code(setter),
      bs => {
        import bs.{graph, idOfFullName}

        val abs = graph.abstractions("p.A.f")
        assert(abs.size == 1)
        assert(abs.head == ReadWriteAbstraction(None, Some("p.A.setF(int)")))

        Rules.abstracter.completeReadWriteAbstraction(graph,
          graph getConcreteNode "p.A.f",
          abs.head.asInstanceOf[ReadWriteAbstraction])
      },code(getter, setter))
  }
}
