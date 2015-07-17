package puck.javaGraph
package nodeKind

import puck.graph.{TypeConstructor, KindType, NodeKind}
import puck.graph.constraints.{SupertypeAbstraction, AbstractionPolicy, DelegationAbstraction}

case object Constructor extends JavaNodeKind {

  override val toString = "Constructor"

  def canContain(k : NodeKind) = k == Definition

  override def abstractionPolicies = Seq(DelegationAbstraction)

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction => Seq(ConstructorMethod, StaticMethod)
    case SupertypeAbstraction => Seq()
      //throw new DGError("Constructor cannot be abstracted by SuperType strategy")
  }

  def kindType: KindType = TypeConstructor
}
