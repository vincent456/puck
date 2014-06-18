package puck.graph

import puck.javaAG.{JavaType, JavaAccessGraph, JavaNodeKind}

/**
 * Created by lorilan on 28/05/14.
 */
class NodeKindSpec extends UnitSpec{

    "A node kind" should "be equal to other instances of the same kind" in {
      val g  = new JavaAccessGraph()

      val k1 = JavaNodeKind.field(new JavaType(g("@primitive.byte")))
      val k2 = JavaNodeKind.field(new JavaType(g("@primitive.char")))

      k1 shouldEqual(k2)
    }
}
