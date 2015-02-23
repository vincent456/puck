package puck.graph

import puck.graph.constraints.{SupertypeAbstraction, DelegationAbstraction, AbstractionPolicy}

/**
 * Created by lorilan on 26/10/14.
 */
trait NodeKind {

  def canContain(k : NodeKind) : Boolean
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)
  def abstractKinds(p : AbstractionPolicy) : Seq[NodeKind]
}
trait AGRoot extends NodeKind {
  def canContain(k: NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) =
    throw new DGError("Root node cannot be abstracted")
}
