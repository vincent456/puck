package puck.javaGraph.commutativity.redirect

import puck.TransfoRulesSpec
import puck.graph.ReadWriteAbstraction
import puck.graph.transformations.rules.Redirection

/**
  * Created by Loïc Girault on 31/08/16.
  */
class RedirectFieldUsesSpec
  extends TransfoRulesSpec {



  scenario("From field to getter"){
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

  scenario("From field to setter"){
    def code(call : String) : String =
      s"""package p;
          |
          |class B {
          |    int f;
          |    void setF(int f){ this.f = f; }
          |}
          |
          |class A {
          |    void m(B b){
          |        b.$call;
          |    }
          |}"""

    compareWithExpectedAndGenerated( code("f = 42"),
      bs => {
        import bs.{graph, idOfFullName}

        val abs = ReadWriteAbstraction(None, Some("p.B.setF(int)"))

        Redirection.redirectUsesAndPropagate(graph.addAbstraction("p.B.f", abs),
          ("p.A.m(B).Definition", "p.B.f"), abs).rvalue
      },
      code("setF(42)"))
  }

  scenario("From field to getter and setter"){
    def code(call : String) : String =
      s"""package p;
          |
          |class B {
          |    int f;
          |    int getF(){ return f; }
          |    void setF(int f){ this.f = f; }
          |}
          |
          |class A {
          |    void m(B b1, B b2){
          |        $call;
          |    }
          |}"""

    compareWithExpectedAndGenerated( code("b1.f = b2.f"),
      bs => {
        import bs.{graph, idOfFullName}

        val abs = ReadWriteAbstraction(Some("p.B.getF()"), Some("p.B.setF(int)"))

        Redirection.redirectUsesAndPropagate(graph.addAbstraction("p.B.f", abs),
          ("p.A.m(B,B).Definition", "p.B.f"), abs).rvalue
      },
      code("b1.setF(b2.getF())"))
  }
}
