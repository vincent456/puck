package puck.graph.immutable

import AccessGraph.NodeId
import puck.graph.AGError
import puck.graph.constraints.{AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
/**
 * Created by lorilan on 26/10/14.
 */
trait NodeKind[K <: NodeKind[K]] {

  def node : NodeId[K]
  def create(id : NodeId[K]) : K
  def canContain(k : K) : Boolean
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction(), DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) : Seq[K]
}

trait AGRoot[K <: NodeKind[K]] extends NodeKind[K] {
  def canContain(k: K) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("Root node cannot be abstracted")
}


trait HasType[K <: NodeKind[K], T <: Type[K, T]] extends NodeKind[K] {
  def typ : T
}
