package puck.graph

/**
 * Created by lorilan on 16/05/14.
 */

sealed abstract class EdgeKind {
  def apply[NK <: NodeKind[NK]](source : AGNode[NK],
                                target: AGNode[NK]) : AGEdge[NK]
}

case class Uses() extends EdgeKind {
  override val toString = "uses"
  def apply[NK <: NodeKind[NK]](source : AGNode[NK],
                                target: AGNode[NK]) = AGEdge.uses(source, target)

}
case class Contains() extends EdgeKind {
  override val toString = "contains"
  def apply[NK <: NodeKind[NK]](source : AGNode[NK],
                                target: AGNode[NK]) = AGEdge.contains(source, target)


}
case class Isa() extends EdgeKind {
  override val toString = "isa"
  def apply[NK <: NodeKind[NK]](source : AGNode[NK],
                                target: AGNode[NK]) = AGEdge.isa(source, target)
}

case class AGEdge[NK <: NodeKind[NK]](kind : EdgeKind,
                                      source : AGNode[NK],
                                      target: AGNode[NK]) {

  /*
   * aliases for readibility
   */
  def user = source
  def usee = target

  def container = source
  def content = target

  def graph = source.graph

  override def equals(obj:Any) : Boolean = obj match {
    case that : AGEdge[_] => this.kind == that.kind && this.source == that.source && this.target == that.target
    case _ => false
  }
  override def hashCode : Int = source.id + target.id

  override def toString : String = {
    kind+ "( " + source + ", " + target + ")"
  }

  def exists = kind match {
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
  def uses[NK <: NodeKind[NK]](source : AGNode[NK], target: AGNode[NK]) = AGEdge(Uses(), source, target)
  def contains[NK <: NodeKind[NK]](source : AGNode[NK], target: AGNode[NK]) = AGEdge(Contains(), source, target)
  def isa[NK <: NodeKind[NK]](source : AGNode[NK], target : AGNode[NK]) = AGEdge(Isa(), source, target)
}
