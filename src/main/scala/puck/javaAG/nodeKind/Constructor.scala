package puck.javaAG
package nodeKind

import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.{AGError, NodeKind}

/**
 * Created by lorilan on 31/07/14.
 */

case object Constructor extends JavaNodeKind {

  override val toString = "Constructor"

  def canContain(k : NodeKind) = false

  override def abstractionPolicies = Seq(DelegationAbstraction)

  def abstractKinds(p : AbstractionPolicy) = p match {
    case DelegationAbstraction => Seq(ConstructorMethod)
    case SupertypeAbstraction => throw new AGError("Constructor cannot be abstracted by SuperType strategy")
  }

}
