package puck.graph

import puck.graph.constraints.{AbstractionPolicy, SupertypeAbstraction, DelegationAbstraction}

/**
 * Created by lorilan on 05/05/14.
 */

trait NodeKind[K <: NodeKind[K]] {

  var node : AGNode[K] = _

  def create() : K
  def canContain(k : K) : Boolean
  def abstractionPolicies : List[AbstractionPolicy] = List(SupertypeAbstraction(), DelegationAbstraction())
  def abstractKinds(p : AbstractionPolicy) : List[K]
  val canBeRootContent = false

 /* def promoteToSuperTypeWherePossible(superType : AGNode[K]){}*/
  def redirectUses(oldUsee : AGNode[K], newUsee : AGNode[K]) {}   //TODO (?) move in HasType

}

trait AGRoot[K <: NodeKind[K]] extends NodeKind[K] {
  override def abstractionPolicies = List()
  def canContain(k: K) = false
  def abstractKinds(p : AbstractionPolicy) =
    throw new AGError("Root node cannot be abstracted")
}

sealed abstract class VanillaKind extends NodeKind[VanillaKind]

case class VanillaNodeKind private[graph]() extends VanillaKind {

  def create() = VanillaNodeKind()

  //change priority order
  override def abstractionPolicies : List[AbstractionPolicy] =
    List(DelegationAbstraction(), SupertypeAbstraction())

  def canContain(k : VanillaKind) : Boolean = {
    k match {
      case VanillaNodeKind() => true
      case _ => false
    }
  }

  def abstractKinds(p : AbstractionPolicy) = List(this)

  override val canBeRootContent = true
}

case class VanillaRoot() extends VanillaKind with AGRoot[VanillaKind]{
  def create() = VanillaRoot()
}


trait HasType[T <: Type] {
  var `type` : T = _
}