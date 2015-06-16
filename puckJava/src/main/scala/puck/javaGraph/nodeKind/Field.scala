package puck.javaGraph
package nodeKind

import puck.graph.NodeKind
import puck.graph.constraints.{SupertypeAbstraction, AbstractionPolicy, DelegationAbstraction}


case object Field extends JavaNodeKind {

  override val toString = "Field"

  override def canBeReadOrWrote = true

  def canContain(k : NodeKind) = false

  // fieldread abstraction type = () -> t
  // fielwrite abstraction type = t -> ()
  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
      case SupertypeAbstraction => Seq()
      case DelegationAbstraction => Seq(Method)
    }


  override def abstractionPolicies = Seq(DelegationAbstraction)
}
