package puck.graph.java

import puck.graph.{AGNode, AccessGraph}

/**
 * Created by lorilan on 07/05/14.
 */
object JavaAccessGraph {
  def apply(): AccessGraph = {
    val g = new AccessGraph(AGNode)

    List(Primitive.voidNode(g),
      Primitive.booleanNode(g),
      Primitive.byteNode(g),
      Primitive.charNode(g),
      Primitive.doubleNode(g),
      Primitive.floatNode(g),
      Primitive.intNode(g),
      Primitive.longNode(g),
      Primitive.shortNode(g),
      Primitive.stringNode(g)) foreach {
      (n : AGNode) => g.nodesByName += ((n.name, n))
                      g.nodesById += ((n.id, n))

    }

    g
  }
}
