package puck.graph.immutable

import puck.graph.AGError
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.immutable.AccessGraph.NodeId

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
    throw new AGError("Root node cannot be abstracted")
}

trait TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: AGNode) : TypeHolder
  def redirectContravariantUses(oldUsee : NodeId, newUsee: AGNode) : TypeHolder
  def mkString(graph : AccessGraph) : String
  def isEmpty = false

}

case object NoType extends TypeHolder {
  def redirectUses(oldUsee : NodeId, newUsee: AGNode) = this
  def redirectContravariantUses(oldUsee : NodeId, newUsee: AGNode) = this
  def mkString(graph : AccessGraph) : String = ""
  override def isEmpty = true
}