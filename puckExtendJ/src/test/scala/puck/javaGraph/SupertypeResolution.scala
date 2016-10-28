package puck.javaGraph

import puck.AcceptanceSpec
import puck.graph.{AccessAbstraction, SupertypeAbstraction}
import puck.graph.transformations.rules.Redirection
/**
  * Created by Loïc Girault on 10/27/16.
  */
class SupertypeResolution extends AcceptanceSpec {

  feature("method redefinition"){

    scenario("overloaded method"){
      val s = new ScenarioFactory(
        s"""package p;
            |
            |interface I {
            |    void m(String s1, String s2);
            |    void m(String s1);
            |}
            |
            |class B implements I {
            |   public void m(String s1, String s2){}
            |   public void m(String s1){}
            |}"""
      )

      import s._
      Redirection.tmAbstraction(s.graph, "p.I", "p.B.m(String)").rvalue shouldBe AccessAbstraction("p.I.m(String)", SupertypeAbstraction)
      Redirection.tmAbstraction(s.graph, "p.I", "p.B.m(String,String)").rvalue shouldBe AccessAbstraction("p.I.m(String,String)", SupertypeAbstraction)
    }
  }

}
