package puck.graph

import puck.javaAG.mutable.{JavaAccessGraph, JavaNamedType}
import puck.javaAG.mutable.nodeKind.JavaNodeKind
import puck.javaAG.JavaDependencyGraph

/**
 * Created by lorilan on 28/05/14.
 */
class NodeKindSpec extends UnitSpec{


    "A node kind" should "be equal to other instances of the same kind" in {
      val g  = new JavaDependencyGraph()

      val k1 = JavaNodeKind.field(new JavaNamedType(g("@primitive.byte")))
      val k2 = JavaNodeKind.field(new JavaNamedType(g("@primitive.char")))

      k1 shouldEqual (k2)
    }
}
