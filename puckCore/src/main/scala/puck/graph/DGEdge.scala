package puck.graph

import puck.graph.DGEdge._


object DGEdge{

  sealed trait EKind {
    def apply(pair : (NodeId, NodeId)) : DGEdge =
      this.apply(pair._1, pair._2)

    def apply(source : NodeId, target: NodeId): DGEdge
  }

//  sealed abstract class UsesKind extends EKind {
//    override def apply(pair : (NodeId, NodeId)) : DGUses =
//      this.apply(pair._1, pair._2)
//
//    override def apply(source : NodeId, target: NodeId): DGUses
//  }
//
//  case object UsesK extends UsesKind {
//    override val toString = "uses"
//
//    def apply(source : NodeId, target: NodeId) =
//      Uses(source, target)
//  }
//
//  case object ParameterizedUsesK extends UsesKind {
//    override val toString = "parUses"
//
//    def apply(source : NodeId, target: NodeId) =
//      ParameterizedUses(source, target)
//  }
//
//  case object ContainsK extends EKind {
//    override val toString = "contains"
//    def apply(source : NodeId, target: NodeId) =
//      Contains(source, target)
//  }
//
//  case object IsaK extends EKind {
//    override val toString = "isa"
//
//    def apply(source : NodeId, target : NodeId) =
//      Isa(source, target)
//  }

  def unapply(e : DGEdge) : Some[(NodeId, NodeId)] =
    Some((e.source, e.target))


  implicit def toPair(e : DGEdge) : NodeIdP = (e.user, e.used)
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



sealed trait UsesKind extends EKind

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

case object Contains extends EKind
case class Contains
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = Contains

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    Contains(source, target)
}

//Special cases of contains for handling in EdgeMap should not appear in the formalization :
case object ContainsParam extends EKind
case class ContainsParam
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = ContainsParam

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    ContainsParam(source, target)
}

case object ContainsDef extends EKind
case class ContainsDef
( source : NodeId,
  target: NodeId)
  extends DGEdge {
  val kind: EKind = ContainsDef

  override def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    ContainsDef(source, target)
}