package puck.javaGraph.nodeKind

import puck.graph.{DGError, NodeKind}
import puck.graph.constraints.AbstractionPolicy

case object Primitive extends TypeKind {
  def canContain(k: NodeKind) = false
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("do not know how to abstract primitive kind")
}
