package puck.javaGraph.nodeKind

import puck.graph.NodeKind
import puck.graph.constraints.AbstractionPolicy

/**
 * Created by lorilan on 31/07/14.
 */
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
  def abstractKinds(p : AbstractionPolicy) = Seq()

}
