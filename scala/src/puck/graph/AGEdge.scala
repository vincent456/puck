package puck.graph

/**
 * Created by lorilan on 16/05/14.
 */

sealed abstract class EdgeKind
case class Undefined() extends EdgeKind
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
    "(" + kind+ ", " + source + ", " + target + ")"
  }

  def create() {
    kind match {
      case Uses() => source.uses_+=(target)
      case Contains() => source.content_+=(target)
      case Isa() => source.superTypes_+=(target)
      case _ => throw new AGError(kind + " edge create not implemented")

    }
  }
  def delete() {
    kind match {
      case Uses() => source.uses_-=(target)
      case Contains() => source.content_-=(target)
      case Isa() => source.superTypes_-=(target)
      case _ => throw new AGError(kind + " edge delete not implemented")

    }
  }
}

object AGEdge{
  def apply(kind : EdgeKind, source : AGNode, target: AGNode) = new AGEdge(kind, source, target)
  def apply(source : AGNode, target: AGNode) = new AGEdge(Undefined(), source, target)

  def uses(source : AGNode, target: AGNode) = apply(Uses(), source, target)
  def contains(source : AGNode,target: AGNode) = apply(Contains(), source, target)
}
