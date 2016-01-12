package puck.graph
import puck.graph.DGEdge._


sealed abstract class EKind {
  def apply(pair : (NodeId, NodeId)) : DGEdge = this.apply(pair._1, pair._2)
  def apply(source : NodeId, target: NodeId): DGEdge
}

object DGEdge{



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

  def pairOfKind(e: DGEdge, k : EKind) : Option[NodeIdP] =
    if(e.kind == k) Some((e.source, e.target))
    else None


}

case object AbstractEdgeKind extends EKind {
  def apply(source: NodeId, target: NodeId): DGEdge = new DGEdge(AbstractEdgeKind, source, target)
  override def toString = "Edge"
}

case class DGEdge(kind : EKind, source : NodeId, target: NodeId) {

  def copy(source : NodeId = source, target : NodeId = target) : DGEdge =
    new DGEdge(kind, source, target)

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



class Uses(source : NodeId, target: NodeId, val accessKind : Option[UsesAccessKind])
  extends DGEdge(Uses, source, target){

  override def toString : String =
    accessKind match {
      case None => super.toString
      case Some(ac) =>
        ac + "-" + kind + "( " + source + ", " + target + ")"
    }

  override def copy(source : NodeId = source, target : NodeId = target) : Uses =
    new Uses(source, target, accessKind)

  def withAccessKind(accessKind: Option[UsesAccessKind]) : Uses =
    new Uses(source, target, accessKind)

  def isDominant(graph : DependencyGraph) : Boolean = graph.typeMemberUsesOf(this).nonEmpty
  def isDominated(graph : DependencyGraph) : Boolean = graph.typeUsesOf(this).nonEmpty
}

case object Uses extends EKind {
  override def apply(source: NodeId, target: NodeId) : Uses = apply(source, target, None)
  def apply(source: NodeId, target: NodeId, accessKind : Option[UsesAccessKind]): Uses = new Uses(source, target, accessKind)

  def unapply(e: DGEdge) : Option[(NodeId, NodeId, Option[UsesAccessKind])] =
    e match {
      case u : Uses => Some((e.source, e.target, u.accessKind))
      case _ => None
    }
}

case object Isa extends EKind {
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(Isa, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, Isa)
}

sealed abstract class ContainsKind extends EKind

case object Contains extends ContainsKind{
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(Contains, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, Contains)
}

//Special cases of contains for handling in EdgeMap should not appear in the formalization :
case object ContainsParam extends ContainsKind {
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(ContainsParam, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, ContainsParam)
}

case object ContainsDef extends ContainsKind {
  def apply(source : NodeId, target: NodeId) : DGEdge = new DGEdge(ContainsDef, source, target)
  def unapply(e: DGEdge) : Option[NodeIdP] = pairOfKind(e, ContainsDef)
}