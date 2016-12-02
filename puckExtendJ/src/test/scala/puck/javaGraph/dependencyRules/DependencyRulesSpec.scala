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

  scenario("error from a marauroa constraint generated example") {
    val s = new ScenarioFactory(
      """package p.pa;
        |import p.pb.B;
        |import p.pb.C;
        |
        |public class A {
        | void ontInit() throws Exception {
        |   B.get().get(C.class).load(this);
        | }
        |}""",

        """package p.pb;
          |public class B {
          | public static B get(){return null;}
          | public <T extends java.lang.Object> T get(Class<T> clazz) { return null;}
          |}
        """,
      """
        |package p.pb;
        |import p.pa.A;
        |public class C {
        | public void load(A a){}
        |}
      """
     )

    import s._
    val cts = s.parseConstraint("hide [p.pb] from [p.pa]")

    //    import puck.graph.ShowDG._
    //    cts.violations(graph).foreach {
    //      u =>
    //      (graph, u).println
    //    }

    cts.violations(graph) should not be empty
  }

}
