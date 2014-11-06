package puck.javaAG.immutable.nodeKind

import puck.graph.AGError
import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable.NodeKind

/**
 * Created by lorilan on 31/07/14.
 */
case object Primitive extends TypeKind {
  def canContain(k: NodeKind) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("do not know how to abstract primitive kind")
}
