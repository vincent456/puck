package puck.javaGraph.dependencyRules

import puck.AcceptanceSpec
import puck.javaGraph.ScenarioFactory

/**
  * Created by Loïc Girault on 19/09/16.
  */
class DependencyRulesSpec extends AcceptanceSpec {



  scenario("synchronized block") {
    val s = new ScenarioFactory(
      """package p;
        |class A { void ma(){} }
        |
        |class B { void m(){
        |   synchronized(this){
        |     A a = new A();
        |     a.ma();
        |   }
        | }
        |}""")

    import s._
    val cts = s.parseConstraint("hide [p.A] from [p.B]")

//    import puck.graph.ShowDG._
//    cts.violations(graph).foreach {
//      u =>
//      (graph, u).println
//    }

    cts.violations(graph) should not be empty


  }

}
