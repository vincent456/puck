package puck.graph

import puck.graph.DGEdge._


object DGEdge{

  sealed abstract class EKind {
    def apply(pair : (NodeId, NodeId)) : DGEdge =
      this.apply(pair._1, pair._2)

    def apply(source : NodeId, target: NodeId): DGEdge
  }

  def unapply(e : DGEdge) : Some[(EKind, NodeId, NodeId)] =
    Some((e.kind, e.source, e.target))

  implicit def toPair(e : DGEdge) : NodeIdP = (e.user, e.used)

  def concreteEdgesFrom(graph : DependencyGraph, virtual : NodeIdP) : List[DGEdge] = {
   val (source, target) = virtual
    val includedInVirtual : NodeIdP => Boolean = {
      case (user, used) => graph.contains_*(source, user) && graph.contains_*(target, used)
    }
    val concreteUses = graph.usesList filter includedInVirtual map {
      case (user, used) => graph.getUsesEdge_!(user, used)
    }

    (graph.isaList filter includedInVirtual map Isa.apply) ++ concreteUses

  }


}

sealed abstract class DGEdge {

  val kind : EKind
  val source : NodeId
  val target: NodeId

  def copy(source : NodeId = source, target : NodeId = target) : DGEdge

  type NIdT = NodeId
  /*
   * aliases for readibility
   */
  def user = source
  def used = target

  def container = source
  def content = target

  def subType = source
  def superType = target

  def selfUse : Boolean = source == target

  /*override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge[_] => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id*/

  override def toString : String = {
    kind + "( " + source + ", " + target + ")"
  }

  def existsIn(graph : DependencyGraph) =
    graph.exists(this)

  def createIn(graph : DependencyGraph, register : Boolean = true) : DependencyGraph =
    graph.addEdge(this, register)

  def deleteIn(graph : DependencyGraph, register : Boolean = true) : DependencyGraph =
    graph.removeEdge(this, register)

  def changeTarget(graph : DependencyGraph, newTarget : NIdT) : DependencyGraph = graph.changeTarget(this, newTarget)
  def changeSource(graph : DependencyGraph, newSource : NIdT) : DependencyGraph = graph.changeSource(this, newSource)


}



sealed abstract class UsesKind extends EKind

sealed abstract class DGUses extends DGEdge{
  override val kind : UsesKind
  override def copy(source : NodeId = source, target : NodeId = target) : DGUses
  def accessKind : Option[UsesAccessKind]
  def isDominant(graph : DependencyGraph) : Boolean = graph.typeMemberUsesOf(this).nonEmpty
  def isDominated(graph : DependencyGraph) : Boolean = graph.typeUsesOf(this).nonEmpty

}

//object Uses extends EKind {
//  override def apply(source: NodeId, target: NodeId): DGEdge =
//    new Uses(source, target)
//}

case object Uses extends UsesKind {
  override def apply(source: NodeId, target: NodeId): Uses = new Uses(source, target)
}

case class Uses
( source : NodeId,
  target: NodeId,
  accessKind : Option[UsesAccessKind] = None
  ) extends DGUses {
  val kind: UsesKind = Uses

  override def toString : String =
  accessKind match {
    case None => super.toString
    case Some(ac) =>
      ac + "-" + kind + "( " + source + ", " + target + ")"
  }

  override def copy(source : NodeId = source, target : NodeId = target) : Uses =
    Uses(source, target, accessKind)
}

case object ParameterizedUses extends UsesKind {
  override def apply(source: NodeId, target: NodeId): ParameterizedUses = new ParameterizedUses(source, target)
}
case class ParameterizedUses
( source : NodeId,
  target: NodeId,
  accessKind : Option[UsesAccessKind] = None
  )
  extends DGUses {
  val kind: UsesKind = ParameterizedUses

  override def copy(source : NodeId = source, target : NodeId = target) : ParameterizedUses =
    ParameterizedUses(source, target, accessKind)
}

case object Isa extends EKind
case class Isa
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = Isa

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    Isa(source, target)

}

sealed abstract class ContainsKind extends EKind

case object Contains extends ContainsKind
case class Contains
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = Contains

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    Contains(source, target)
}

//Special cases of contains for handling in EdgeMap should not appear in the formalization :
case object ContainsParam extends ContainsKind
case class ContainsParam
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = ContainsParam

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    ContainsParam(source, target)
}

case object ContainsDef extends ContainsKind
case class ContainsDef
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = ContainsDef

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    ContainsDef(source, target)
}