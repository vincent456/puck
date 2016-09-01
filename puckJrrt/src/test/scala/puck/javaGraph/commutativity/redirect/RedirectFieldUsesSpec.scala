package puck.javaGraph.commutativity.redirect

import puck.TransfoRulesSpec
import puck.graph.ReadWriteAbstraction
import puck.graph.transformations.rules.Redirection

/**
  * Created by Loïc Girault on 31/08/16.
  */
class RedirectFieldUsesSpec
  extends TransfoRulesSpec {



  scenario("From field to method delegate"){
    def code(call : String) : String =
      s"""package p;
          |
          |class B {
          |    int f;
          |    int getF(){ return f; }
          |}
          |
          |class A {
          |    void m(B b){
          |        int i = b.$call;
          |    }
          |}"""

    compareWithExpectedAndGenerated( code("f"),
      bs => {
        import bs.{graph, idOfFullName}

        val abs = ReadWriteAbstraction(Some("p.B.getF()"), None)

        Redirection.redirectUsesAndPropagate(graph.addAbstraction("p.B.f", abs),
          ("p.A.m(B).Definition", "p.B.f"), abs).rvalue
      },
      code("getF()"))
  }

}
