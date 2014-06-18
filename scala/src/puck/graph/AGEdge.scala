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

  /*
   * aliases for readibility
   */
  def user = source
  def usee = target

  def container = source
  def content = target

  override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id

  override def toString : String = {
    "(" + kind+ ", " + source + ", " + target + ")"
  }

  def exists = kind match {
    case Uses() => source uses target
    case Contains() => source contains target
    case Isa() => source isa target
    case Undefined() => throw new AGError("undefined type edge cannot exist")
  }

  def create() {
    //println("creating "+ this)
    kind match {
      case Uses() => source.uses_+=(target)
      case Contains() => source.content_+=(target)
      case Isa() => source.superTypes_+=(target)
      case Undefined() => throw new AGError("cannot create arc with undefined kind")

    }
  }
  def delete() {
    //println("deleting "+ this)
    kind match {
      case Uses() => source.uses_-=(target)
      case Contains() => source.content_-=(target)
      case Isa() => source.superTypes_-=(target)
      case Undefined() => throw new AGError("cannot delete arc with undefined kind")

    }
  }
}

object AGEdge{
  def apply(kind : EdgeKind, source : AGNode, target: AGNode) = new AGEdge(kind, source, target)
  def apply(source : AGNode, target: AGNode) : AGEdge = apply(Undefined(), source, target)

  def uses(source : AGNode, target: AGNode) = apply(Uses(), source, target)
  def contains(source : AGNode, target: AGNode) = apply(Contains(), source, target)
  def isa(source : AGNode, target : AGNode) = apply(Isa(), source, target)
}
