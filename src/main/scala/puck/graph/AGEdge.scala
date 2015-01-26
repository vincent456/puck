package puck.graph

/**
 * Created by lorilan on 1/8/15.
 */

sealed abstract class EdgeKind {
  def apply(source : NodeId, target: NodeId) : AGEdge
}

case object Uses extends EdgeKind {
  override val toString = "uses"
  def apply(source : NodeId, target: NodeId) =
    AGEdge.uses(source, target)

}
case object Contains extends EdgeKind {
  override val toString = "contains"
  def apply(source : NodeId, target: NodeId) =
    AGEdge.contains(source, target)


}
case object Isa extends EdgeKind {
  override val toString = "isa"
  def apply(source : NodeId, target: NodeId) =
    AGEdge.isa(source, target)
}

object AGEdge{
  def uses(pair : (NodeId, NodeId)) =
    AGEdge(Uses, pair._1, pair._2)

  def uses(source : NodeId, target: NodeId) =
    AGEdge(Uses, source, target)

  def contains(source : NodeId, target: NodeId) =
    AGEdge(Contains, source, target)

  def isa(source : NodeId, target : NodeId) =
    AGEdge(Isa, source, target)
}

case class AGEdge
( kind : EdgeKind,
  source : NodeId,
  target: NodeId) {

  type NIdT = NodeId
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
  def mkString(graph : DependencyGraph) : String =
    kind + "( " + graph.getNode(source) + ", " + graph.getNode(target) + ")"


  def exists(graph : DependencyGraph) = kind match {
    case Uses => graph.uses(source, target)
    case Contains => graph.contains(source, target)
    case Isa => graph.isa(source, target)
  }

  def create(graph : DependencyGraph, register : Boolean = true) : DependencyGraph = {
    //println("creating "+ this)
    kind match {
      case Uses => graph.addUses(source, target, register)
      case Contains => graph.addContains(source, target, register)
      case Isa => graph.addIsa(source, target, register)

    }
  }
  def delete(graph : DependencyGraph, register : Boolean = true)  : DependencyGraph = {
    //println("deleting "+ this)
    kind match {
      case Uses => graph.removeUses(source, target, register)
      case Contains => graph.removeContains(source, target, register)
      case Isa => graph.removeIsa(source, target, register)
    }
  }

  def changeTarget(graph : DependencyGraph, newTarget : NIdT) : DependencyGraph = graph.changeTarget(this, newTarget)

  def changeSource(graph : DependencyGraph, newSource : NIdT) : DependencyGraph = graph.changeSource(this, newSource)

  def isDominant(graph : DependencyGraph) : Boolean = graph.usesDominatedBy(this.source, this.target).nonEmpty
  def isDominated(graph : DependencyGraph) : Boolean = graph.usesDominating(this.source, this.target).nonEmpty
}