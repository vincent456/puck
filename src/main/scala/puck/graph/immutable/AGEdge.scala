package puck.graph.immutable

import AccessGraph.NodeId

/**
 * Created by lorilan on 27/10/14.
 */
sealed abstract class EdgeKind {
  def apply[NK <: NodeKind[NK]](source : NodeId[NK],
                                target: NodeId[NK]) : AGEdge[NK]
}

case object Uses extends EdgeKind {
  override val toString = "uses"
  def apply[NK <: NodeKind[NK]](source : NodeId[NK],
                                target: NodeId[NK]) =
    AGEdge.uses[NK](source, target)

}
case object Contains extends EdgeKind {
  override val toString = "contains"
  def apply[NK <: NodeKind[NK]](source : NodeId[NK],
                                target: NodeId[NK]) =
    AGEdge.contains(source, target)


}
case object Isa extends EdgeKind {
  override val toString = "isa"
  def apply[NK <: NodeKind[NK]](source : NodeId[NK],
                                target: NodeId[NK]) =
    AGEdge.isa(source, target)
}

case class AGEdge[NK <: NodeKind[NK]]
( kind : EdgeKind,
  source : NodeId[NK],
  target: NodeId[NK]) {

  type GraphT[T] = AccessGraph[NK,T]
  type NIdT = NodeId[NK]
  /*
   * aliases for readibility
   */
  def user = source
  def usee = target

  def container = source
  def content = target

  /*override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge[_] => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id*/

  override def toString : String = {
    kind + "( " + source + ", " + target + ")"
  }

  def exists[T](graph : GraphT[T]) = kind match {
    case Uses => graph.uses(source, target)
    case Contains => graph.contains(source, target)
    case Isa => graph.isa(source, target)
  }

  def create[T](graph : GraphT[T], register : Boolean = true) : GraphT[T] = {
    //println("creating "+ this)
    kind match {
      case Uses => graph.addUses(source, target, register)
      case Contains => graph.addContains(source, target, register)
      case Isa => graph.addIsa(source, target, register)

    }
  }
  def delete[T](graph : GraphT[T], register : Boolean = true)  : GraphT[T] = {
    //println("deleting "+ this)
    kind match {
      case Uses => graph.removeUses(source, target, register)
      case Contains => graph.removeContains(source, target, register)
      case Isa => graph.removeIsa(source, target, register)
    }
  }

  def changeTarget[T](graph : GraphT[T], newTarget : NIdT) : GraphT[T] = graph.changeTarget(this, newTarget)

  def changeSource[T](graph : GraphT[T], newSource : NIdT) : GraphT[T] = graph.changeSource(this, newSource)

  def isDominant[T](graph : GraphT[T]) : Boolean = graph.dominatedUses(this.source, this.target).nonEmpty
  def isDominated[T](graph : GraphT[T]) : Boolean = graph.dominantUses(this.source, this.target).nonEmpty
}

object AGEdge{
  def uses[NK <: NodeKind[NK]](pair : (NodeId[NK], NodeId[NK])) =
    AGEdge[NK](Uses, pair._1, pair._2)

  def uses[NK <: NodeKind[NK]](source : NodeId[NK],
                               target: NodeId[NK]) =
      AGEdge[NK](Uses, source, target)

  def contains[NK <: NodeKind[NK]](source : NodeId[NK],
                                   target: NodeId[NK]) =
    AGEdge[NK](Contains, source, target)

  def isa[NK <: NodeKind[NK]](source : NodeId[NK],
                              target : NodeId[NK]) =
    AGEdge[NK](Isa, source, target)
}