package puck.graph

/**
 * Created by lorilan on 16/05/14.
 */

sealed abstract class EdgeKind
case class Uses() extends EdgeKind
case class Contains() extends EdgeKind
case class Isa() extends EdgeKind

class AGEdge private (val kind : EdgeKind, val source : AGNode, val target: AGNode){

  override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id

  override def toString : String = {
    "(" + kind+ ", " + source.fullName + ", " + target.fullName + ")"
  }
}

object AGEdge{
  def apply(kind : EdgeKind, source : AGNode, target: AGNode) = new AGEdge(kind, source, target)
  def uses(source : AGNode, target: AGNode) = apply(Uses(), source, target)
  def contains(source : AGNode,target: AGNode) = apply(Contains(), source, target)
}
