package puck.javaGraph.commutativity

import puck.TransfoRulesSpec
import puck.jastadd.ExtendJGraphUtils._

/**
  * Created by lorilan on 5/7/16.
  */
class IntroSpec extends TransfoRulesSpec {

  feature("Intro initializer"){
    scenario("one constructor one initialized field"){
      compareWithExpectedAndGenerated(
        """package p;
          |
          |class F{}
          |
          |public class A {
          |    private F f = new F();
          |    public A(){}
          |}""",
        bs => {
          import bs.{graph, idOfFullName}
          Rules.intro.initializer(graph, "p.A")._2
        },
        """package p;
          |
          |class F{}
          |
          |public class A {
          |
          |    private F f;
          |
          |    public A(){ init(); }
          |
          |    void init(){ f = new F(); }
          |
          |}""")
    }
  }

}
