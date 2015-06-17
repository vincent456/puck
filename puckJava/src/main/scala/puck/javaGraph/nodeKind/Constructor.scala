package puck.javaGraph
package nodeKind

import puck.graph.NodeKind
import puck.graph.constraints.{SupertypeAbstraction, AbstractionPolicy, DelegationAbstraction}

case object Constructor extends JavaNodeKind {

  override val toString = "Constructor"

  def canContain(k : NodeKind) = false

  override def abstractionPolicies = Seq(DelegationAbstraction)

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction => Seq(ConstructorMethod)
    case SupertypeAbstraction => Seq()
      //throw new DGError("Constructor cannot be abstracted by SuperType strategy")
  }

}
