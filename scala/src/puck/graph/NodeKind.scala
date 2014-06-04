package puck.graph

import puck.graph.constraints.{Supertype, Delegation, AbstractionPolicy}

/**
 * Created by lorilan on 05/05/14.
 */

abstract class NodeKind {
  def canContain(k : NodeKind) : Boolean
  def abstractionPolicies : List[AbstractionPolicy] = List(Supertype(), Delegation())
  def abstractKinds(p : AbstractionPolicy) : List[NodeKind]
  def canBeRootContent = false
}
case class AGRoot private[graph]() extends NodeKind{

  override def abstractionPolicies = List()
  def canContain(k: NodeKind) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("Root node cannot be abstracted")

}
case class VanillaKind private[graph]() extends NodeKind{

  //change priority order
  override def abstractionPolicies : List[AbstractionPolicy] =
    List(Delegation(), Supertype())

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case VanillaKind() => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = List(this)

  override def canBeRootContent = true
}

trait HasType[T<:Type] {
  var `type` : T = _
}