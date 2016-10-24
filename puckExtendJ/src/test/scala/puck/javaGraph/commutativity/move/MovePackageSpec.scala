package puck.javaGraph.commutativity.move

import puck.TransfoRulesSpec
import puck.graph.Contains
import puck.graph.transformations.rules.Move

/**
  * Created by Loïc Girault on 25/07/16.
  */
class MovePackageSpec
  extends TransfoRulesSpec {

  scenario("Move top level class") {
    compareWithExpectedAndGenerated(
      Seq("""package p1.p2;
         public class A{}
        """,
        """package p3.p4;
           import p1.p2.A;
         class B{ A a = new A();}
        """
      ),
      bs => {
        import bs.{graph, idOfFullName}
        Move.staticDecl(graph, "p1.p2", "p3").rvalue
          .removeNode("p1")._2
            .removeEdge(Contains(graph.rootId, "p1"))
      },
      Seq("""package p3.p2;
         public class A{}
          """,
        """package p3.p4;
           import p3.p2.A;
         class B{ A a = new A();}
        """
      ))
  }

}