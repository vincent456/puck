package puck.javaGraph.commutativity.move

import puck.TransfoRulesSpec
import puck.graph.transformations.rules.Move

/**
  * Created by Loïc Girault on 30/08/16.
  */
class MoveStaticFieldSpec
  extends TransfoRulesSpec  {

  scenario("Move field between classes in same package") {
    compareWithExpectedAndGenerated(
      """package p;
        |
        |class A { static int i = 42; }
        |
        |class B {
        |    public static void m(){
        |         int j = A.i;
        |    }
        |}
        |
        |class C { } """,
      s => {
        import s.{graph, idOfFullName}
        Move.staticDecl(graph, "p.A.i", "p.C").rvalue
      },
      """package p;
        |
        |class A {  }
        |
        |class B {
        |    public static void m(){
        |         int j = C.i;
        |    }
        |}
        |
        |class C { static int i = 42; } """
    )
  }

}
