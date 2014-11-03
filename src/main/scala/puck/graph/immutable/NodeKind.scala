package puck.graph.immutable

import puck.graph.AGError
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 26/10/14.
 */
trait NodeKind[K <: NodeKind[K]] {

  def canContain(k : K) : Boolean
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)
  def abstractKinds(p : AbstractionPolicy) : Seq[K]
}

trait AGRoot[K <: NodeKind[K]] extends NodeKind[K] {
  def canContain(k: K) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("Root node cannot be abstracted")
}

trait TypeHolder[K <: NodeKind[K]] {
  def redirectUses(oldUsee : NodeId[K], newUsee: AGNode[K, _]) : TypeHolder[K]
  def redirectContravariantUses(oldUsee : NodeId[K], newUsee: AGNode[K, _]) : TypeHolder[K]
  def mkString(graph : AccessGraph[K,_]) : String

}

case class NoType[K <: NodeKind[K]]() extends TypeHolder[K] {
  def redirectUses(oldUsee : NodeId[K], newUsee: AGNode[K, _]) = this
  def redirectContravariantUses(oldUsee : NodeId[K], newUsee: AGNode[K, _]) = this
  def mkString(graph : AccessGraph[K,_]) : String = ""
}