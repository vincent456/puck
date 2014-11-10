package puck.graph.immutable

/**
 * Created by lorilan on 26/10/14.
 */

import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable.AccessGraph.{Mutability, NodeId}

trait AGNodeBuilder {
  def apply(graph : AccessGraph,
            id : NodeId,
            name : String,
            kind : NodeKind,
            styp : TypeHolder,
            isMutable : Mutability,
            t : Hook) : AGNode

  def createT(kind : NodeKind) : Hook

  def rootKind : NodeKind
  def kinds : Seq[NodeKind]
}

import scala.language.existentials
class AGNode
( val graph : AccessGraph,
  val id : NodeId,
  val name : String,
  val kind : NodeKind,
  val styp : TypeHolder,
  val isMutable : Boolean,
  val t : Any){


  type NIdT = NodeId

  def canContain(otherId : NIdT) : Boolean = {
    val n = graph.getNode(otherId)
    !graph.contains_*(otherId, id) && // no cycle !
      (this.kind canContain n.kind) &&
      this.isMutable
  }

  override def toString = id + " - " + kind +" " +name + styp.mkString(graph) + " (" + t +")"

  def container = graph.container(id)
  def content = graph.content(id)

  def users = graph.users(id)
  def used = graph.usedBy(id)

  def directSuperTypes = graph.directSuperTypes(id)
  def directSubTypes = graph.directSubTypes(id)

  def subTypes = graph.subTypes(id)
  def isSuperTypeOf(subCandidate : NIdT) = graph.isSuperTypeOf(id, subCandidate)

  def isRoot = graph.container(id) == id

  def isa( n : NIdT ) = graph.isa(id, n)

  def containerPath  : Seq[NIdT] = {
    def aux(current : NIdT, acc : Seq[NIdT]) : Seq[NIdT] =
      if(graph.isRoot(current)) current +: acc
      else aux(graph.container(current), current +: acc)

    aux(id, Seq())
  }

  def abstractions :  Iterable[(NIdT, AbstractionPolicy)] = graph.abstractions(id)


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

  def wrongUsers : Seq[NIdT] = graph.wrongUsers(id)
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


  private def outgoingDependencies(root : NIdT, acc0 : Set[AGEdge]) : Set[AGEdge]= {
    val acc1 = graph.usedBy(id).foldLeft(acc0){
      (acc, usee) =>
        if(graph.contains_*(root,usee)) acc
        else acc + AGEdge.uses(this.id, usee)
    }
    content.foldLeft(acc1){(acc, child) => graph.getNode(child).outgoingDependencies(root, acc)}
  }

  def outgoingDependencies : Set[AGEdge] = outgoingDependencies(this.id, Set[AGEdge]())

  private def incomingDependencies(root : NIdT, acc0 : Set[AGEdge]) : Set[AGEdge]= {
    val acc1 = graph.users(id).foldLeft(acc0){
      (acc, user) =>
        if(graph.contains_*(root, user)) acc
        else acc + AGEdge.uses(user, this.id)
    }
    content.foldLeft(acc1){(acc, child) => graph.getNode(child).incomingDependencies(root, acc)}
  }

  def incomingDependencies : Set[AGEdge] = incomingDependencies(this.id, Set[AGEdge]())


  private def internalDependencies(root : NIdT, acc0 : Set[AGEdge]) : Set[AGEdge]= {

    val acc1 = graph.usedBy(id).foldLeft(acc0) {
      (acc, usee) =>
        if (graph.contains_*(root,usee))
          acc + AGEdge.uses(id, usee)
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

  def internalDependencies : Set[AGEdge] = internalDependencies(this.id, Set[AGEdge]())


  /*def provides(other : AGNode) = {
    val these0 = Set[AGNode]() ++ this.iterator
    val others0 = Set[AGNode]() ++ other.iterator

    val these = these0 -- others0
    val others = others0 -- these0
    these.exists { t => others.exists(o => o uses t) }
  }*/


  def provides(other : NIdT) = {
    val these = graph.subTree(id)
    val others = graph.subTree(other)

    these.exists { t =>
      others.exists { o =>
        graph.uses(o, t) &&
          !(graph.contains_*(other, o) && graph.contains_*(other, t))
      }
    }
  }

  private def connection( f : AGNode => Boolean) = {
    graph.nodes.foldLeft(Set[NIdT]()){ (acc, n) =>
      if(n.id == this.id || n.kind != this.kind) acc
      else if(f(n)) acc + n.id
      else acc
    }
  }

  def providers : Set[NIdT] = connection { n => n provides this.id}
  def clients : Set[NIdT] = connection{ n => this provides n.id}

  def cohesion : Double = {
    val intd = internalDependencies.size
    intd.toDouble / (outgoingDependencies.size + incomingDependencies.size + intd).toDouble
  }

  def coupling : Double = {
    val dependencies = outgoingDependencies.size + incomingDependencies.size + internalDependencies.size
    1 - (providers ++ clients).size.toDouble / dependencies.toDouble
  }

}
