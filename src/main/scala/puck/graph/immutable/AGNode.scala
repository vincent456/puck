package puck.graph.immutable

/**
 * Created by lorilan on 26/10/14.
 */

import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable.AccessGraph.{Mutability, NodeId}

trait AGNodeBuilder[Kind <: NodeKind[Kind], T] {
  def apply(graph : AccessGraph[Kind, T],
            id : NodeId[Kind],
            name : String,
            kind : Kind,
            styp : TypeHolder[Kind],
            isMutable : Mutability,
            t : T) : AGNode[Kind, T]

  def createT() : T

  def rootKind : Kind
  def kinds : Seq[Kind]
}

import scala.language.existentials
class AGNode[Kind <: NodeKind[Kind], T]
( val graph : AccessGraph[Kind, T],
  val id : NodeId[Kind],
  val name : String,
  val kind : Kind,
  val styp : TypeHolder[Kind],
  val isMutable : Boolean,
  val t : T){


  type NodeIdT = NodeId[Kind]

  def canContain(otherId : NodeIdT) : Boolean = {
    val n = graph.getNode(otherId)
    !graph.contains_*(otherId, id) && // no cycle !
      (this.kind canContain n.kind) &&
      this.isMutable
  }

  override def toString = name + " : " + kind.toString + "(" + id +")"

  def container = graph.container(id)
  def content = graph.content(id)

  def users = graph.users(id)

  def directSuperTypes = graph.directSuperTypes(id)
  def directSubTypes = graph.directSubTypes(id)

  def isRoot = graph.container(id) == id

  def isa( n : NodeIdT ) = graph.isa(id, n)

  def containerPath  : Seq[NodeIdT] = {
    def aux(current : NodeIdT, acc : Seq[NodeIdT]) : Seq[NodeIdT] =
      if(graph.isRoot(current)) current +: acc
      else aux(graph.container(current), current +: acc)

    aux(id, Seq())
  }

  def abstractions :  Iterable[(NodeIdT, AbstractionPolicy)] = graph.abstractions(id)


  def fullName: String = {
    /*if (isRoot) nameTypeString
      else {*/
    val path = containerPath.map{n => graph.getNode(n).nameTypeString}

    (if (path.head == AccessGraph.rootName)
      path.tail
    else
      AccessGraph.unrootedStringId +: path ).mkString(AccessGraph.scopeSeparator)
  }
  def nameTypeString : String = name + styp.mkString(graph)

  def wrongUsers : Seq[NodeIdT] = graph.wrongUsers(id)
  def isWronglyContained : Boolean = graph.isWronglyContained(id)

  /*def distance(other : AGNode[Kind]) = {
    if(this == other) 0
    else if(this contains_*  other)
      other.containerPath(this).length - 1
    else if (other contains_* this)
      this.containerPath(other).length  - 1
    else {
      val thisPathToRoot = this.containerPath.reverse
      val otherPathToRoot = other.containerPath.reverse
      println(thisPathToRoot)
      println(otherPathToRoot)
      thisPathToRoot.foldLeft[Option[AGNode[Kind]]](None) {
        case (sn@Some(_), _) => sn
        case (None, n) =>
          if (otherPathToRoot contains n)
            Some(n)
          else
            None

      } match {
        case None =>
          Int.MaxValue
        case Some(commonAncestor) =>
          val count: ((Int, Boolean), AGNode[Kind]) => (Int, Boolean) = {
            case ((i, alreadyFound), n) =>
              if (alreadyFound || n == commonAncestor) (i, true)
              else (i + 1, false)
          }

          (thisPathToRoot.foldLeft((0, false))(count),
            otherPathToRoot.foldLeft((0, false))(count)) match {
            case ((i, _), (j, _)) => i + j
          }
      }
    }

  }*/


  private def outgoingDependencies(root : NodeIdT, acc0 : Set[AGEdge[Kind]]) : Set[AGEdge[Kind]]= {
    val acc1 = graph.usedBy(id).foldLeft(acc0){
      (acc, usee) =>
        if(graph.contains_*(root,usee)) acc
        else acc + AGEdge.uses[Kind](this.id, usee)
    }
    content.foldLeft(acc1){(acc, child) => graph.getNode(child).outgoingDependencies(root, acc)}
  }

  def outgoingDependencies : Set[AGEdge[Kind]] = outgoingDependencies(this.id, Set[AGEdge[Kind]]())

  private def incomingDependencies(root : NodeIdT, acc0 : Set[AGEdge[Kind]]) : Set[AGEdge[Kind]]= {
    val acc1 = graph.users(id).foldLeft(acc0){
      (acc, user) =>
        if(graph.contains_*(root, user)) acc
        else acc + AGEdge.uses[Kind](user, this.id)
    }
    content.foldLeft(acc1){(acc, child) => graph.getNode(child).incomingDependencies(root, acc)}
  }

  def incomingDependencies : Set[AGEdge[Kind]] = incomingDependencies(this.id, Set[AGEdge[Kind]]())


  private def internalDependencies(root : NodeIdT, acc0 : Set[AGEdge[Kind]]) : Set[AGEdge[Kind]]= {

    val acc1 = graph.usedBy(id).foldLeft(acc0) {
      (acc, usee) =>
        if (graph.contains_*(root,usee))
          acc + AGEdge.uses[Kind](id, usee)
        else acc
    }
    /* not necessary
        val acc2 = users.foldLeft(acc1){
          (acc, user) =>
            if(root contains_* user)
              acc + AGEdge.uses(user, this)
            else acc
        }
    */

    content.foldLeft(acc1){(acc, child) => graph.getNode(child).internalDependencies(root, acc)}
  }

  def internalDependencies : Set[AGEdge[Kind]] = internalDependencies(this.id, Set[AGEdge[Kind]]())


  /*def provides(other : AGNode[Kind]) = {
    val these0 = Set[AGNode[Kind]]() ++ this.iterator
    val others0 = Set[AGNode[Kind]]() ++ other.iterator

    val these = these0 -- others0
    val others = others0 -- these0
    these.exists { t => others.exists(o => o uses t) }
  }*/


  def provides(other : NodeIdT) = {
    val these = graph.subTree(id)
    val others = graph.subTree(other)

    these.exists { t =>
      others.exists { o =>
        graph.uses(o, t) &&
          !(graph.contains_*(other, o) && graph.contains_*(other, t))
      }
    }
  }

  private def connection( f : AGNode[Kind, T] => Boolean) = {
    graph.nodes.foldLeft(Set[NodeIdT]()){ (acc, n) =>
      if(n.id == this.id || n.kind != this.kind) acc
      else if(f(n)) acc + n.id
      else acc
    }
  }

  def providers : Set[NodeIdT] = connection { n => n provides this.id}
  def clients : Set[NodeIdT] = connection{ n => this provides n.id}

  def cohesion : Double = {
    val intd = internalDependencies.size
    intd.toDouble / (outgoingDependencies.size + incomingDependencies.size + intd).toDouble
  }

  def coupling : Double = {
    val dependencies = outgoingDependencies.size + incomingDependencies.size + internalDependencies.size
    1 - (providers ++ clients).size.toDouble / dependencies.toDouble
  }

}
