package puck.graph.immutable

import AccessGraph.NodeId

/**
 * Created by lorilan on 27/10/14.
 */
sealed abstract class EdgeKind {
  def apply[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                                source : NodeId[NK],
                                target: NodeId[NK]) : AGEdge[NK]
}

case class Uses() extends EdgeKind {
  override val toString = "uses"
  def apply[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                                source : NodeId[NK],
                                target: NodeId[NK]) =
    AGEdge.uses(graph, source, target)

}
case class Contains() extends EdgeKind {
  override val toString = "contains"
  def apply[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                                source : NodeId[NK],
                                target: NodeId[NK]) =
    AGEdge.contains(graph, source, target)


}
case class Isa() extends EdgeKind {
  override val toString = "isa"
  def apply[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                                source : NodeId[NK],
                                target: NodeId[NK]) =
    AGEdge.isa(graph, source, target)
}

case class AGEdge[NK <: NodeKind[NK]]
( graph : AccessGraph[NK],
  kind : EdgeKind,
  source : NodeId[NK],
  target: NodeId[NK]) {

  /*
   * aliases for readibility
   */
  def user = source
  def usee = target

  def container = source
  def content = target

  def dominates(e : AGEdge[NK]) : Boolean =
    graph.dominates((this.source, this.target), (e.source, e.target))

  def isDominatedBy(e : AGEdge[NK]) : Boolean =
    graph.dominates((e.source, e.target), (this.source, this.target))

  /*override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge[_] => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id*/

  override def toString : String = {
    kind + "( " + source + ", " + target + ")"
  }

  /*def exists = kind match {
    case Uses() => source uses target
    case Contains() => source contains target
    case Isa() => source isa target
  }

  def create(register : Boolean = true) {
    //println("creating "+ this)
    kind match {
      case Uses() => source.uses += (target, register)
      case Contains() => source.content += (target, register)
      case Isa() => source.superTypes_+=(target, register)

    }
  }
  def delete(register : Boolean = true) {
    //println("deleting "+ this)
    kind match {
      case Uses() => source.uses -= (target, register)
      case Contains() => source.content -= (target, register)
      case Isa() => source.superTypes_-=(target, register)
    }
  }

  def changeTarget(newTarget : AGNode[NK]) = {
    this.delete(register = false)
    val newEdge = new AGEdge(this.kind, this.source, newTarget)
    this.source.graph.transformations.changeEdgeTarget(this, newTarget, withMerge = newEdge.exists)
    newEdge.create(register = false)
    newEdge
  }

  def changeSource(newSource : AGNode[NK]) = {
    this.delete(register = false)
    val newEdge = new AGEdge(this.kind, newSource, this.target)
    this.source.graph.transformations.changeEdgeSource(this, newSource, withMerge = newEdge.exists)
    newEdge.create(register = false)
    newEdge
  }

  def isDominant : Boolean = {
    source.graph.sideUses.get(this) match {
      case Some(_) => true
      case None => false
    }
  }
  def isDominated : Boolean = {
    source.graph.primaryUses.get(this) match {
      case Some(_) => true
      case None => false
    }
  }*/
}

object AGEdge{
  def uses[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                               source : NodeId[NK],
                               target: NodeId[NK]) =
      AGEdge(graph, Uses(), source, target)

  def contains[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                                   source : NodeId[NK],
                                   target: NodeId[NK]) =
    AGEdge(graph, Contains(), source, target)

  def isa[NK <: NodeKind[NK]](graph : AccessGraph[NK],
                              source : NodeId[NK],
                              target : NodeId[NK]) =
    AGEdge(graph, Isa(), source, target)
}