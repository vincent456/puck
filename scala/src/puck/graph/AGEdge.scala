package puck.graph

/**
 * Created by lorilan on 16/05/14.
 */

sealed abstract class EdgeKind
case class Undefined() extends EdgeKind
case class Uses() extends EdgeKind
case class Contains() extends EdgeKind
case class Isa() extends EdgeKind

case class AGEdge[NK <: NodeKind[NK]](kind : EdgeKind,
                                      source : AGNode[NK],
                                      target: AGNode[NK]){

  /*
   * aliases for readibility
   */
  def user = source
  def usee = target

  def container = source
  def content = target

  override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge[_] => this.kind == that.kind && this.source == that.source && this.target == that.target
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

  def create(register : Boolean = true) {
    //println("creating "+ this)
    kind match {
      case Uses() => source.uses_+=(target, register)
      case Contains() => source.content_+=(target, register)
      case Isa() => source.superTypes_+=(target, register)
      case Undefined() => throw new AGError("cannot create arc with undefined kind")

    }
  }
  def delete(register : Boolean = true) {
    //println("deleting "+ this)
    kind match {
      case Uses() => source.uses_-=(target, register)
      case Contains() => source.content_-=(target, register)
      case Isa() => source.superTypes_-=(target, register)
      case Undefined() => throw new AGError("cannot delete arc with undefined kind")

    }
  }

  def changeTarget(newTarget : AGNode[NK]) = {
    this.delete(register = false)
    val newEdge = new AGEdge(this.kind, this.source, newTarget)
    newEdge.create(register = false)
    this.source.graph.transformations.addChangeEdgeTarget(this, newTarget)
    newEdge
  }

  def changeSource(newSource : AGNode[NK]) = {
    this.delete(register = false)
    val newEdge = new AGEdge(this.kind, newSource, this.target)
    newEdge.create(register = false)
    this.source.graph.transformations.addChangeEdgeSource(this, newSource)
    newEdge
  }

  def isDominant : Boolean = {
    source.sideUses.get(target) match {
      case Some(_) => true
      case None => false
    }
  }
  def isDominated : Boolean = {
    source.primaryUses.get(target) match {
      case Some(_) => true
      case None => false
    }
  }
}

object AGEdge{
  def apply[NK <: NodeKind[NK]](source : AGNode[NK], target: AGNode[NK]) : AGEdge[NK] = AGEdge(Undefined(), source, target)

  def uses[NK <: NodeKind[NK]](source : AGNode[NK], target: AGNode[NK]) = AGEdge(Uses(), source, target)
  def contains[NK <: NodeKind[NK]](source : AGNode[NK], target: AGNode[NK]) = AGEdge(Contains(), source, target)
  def isa[NK <: NodeKind[NK]](source : AGNode[NK], target : AGNode[NK]) = AGEdge(Isa(), source, target)
}
