package puck.javaGraph
package nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.{DGError, NodeKind}

/**
 * Created by lorilan on 31/07/14.
 */

case object Constructor extends JavaNodeKind {

  override val toString = "Constructor"

  def canContain(k : NodeKind) = false

  override def isTypeConstructor : Boolean = true

  override def abstractionPolicies = Seq(DelegationAbstraction)

  def abstractKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction => Seq(ConstructorMethod)
    case SupertypeAbstraction => throw new DGError("Constructor cannot be abstracted by SuperType strategy")
  }

}
