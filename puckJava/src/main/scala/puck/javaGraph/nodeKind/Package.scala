package puck.javaGraph.nodeKind

import puck.graph.NodeKind
import puck.graph.constraints.AbstractionPolicy

case object Package extends JavaNodeKind {
  override val toString = "Package"

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
           | Class
           | Interface => true
      case _ => false
    }
  }

  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()

}
