package puck.graph.transformations

import puck.graph.{DependencyGraph, _}


/**
  * Created by Loïc Girault on 24/08/16.
  */

sealed abstract class Mutability {
  def opposite : Mutability
}
case object Mutable extends Mutability {
  override val toString = "mutable"
  def opposite = Immutable
}
case object Immutable extends Mutability {
  override val toString = "immutable"
  def opposite = Mutable
}

object MutabilitySet {

  type T = Set[NodeId]

  def allMutable(graph : DependencyGraph) : T = Set[NodeId]().setMutability(graph.nodesId, Mutable)

  def allImmutable(graph : DependencyGraph) : T = Set[NodeId]()

  def setMutableWithName
  (graph : DependencyGraph, mutableSet : T,
   nodeId : NodeId, name : Seq[String], m : Mutability) : T =
    name match {
      case Nil => mutableSet
      case hd +: tl =>
        graph.content(nodeId) find (graph.getConcreteNode(_).name == hd) match {
          case None => mutableSet
          case Some(nid) =>
            setMutableWithName(graph, mutableSet.setMutability(nid, m), nid, tl, m)
        }
    }

  implicit class MutabilitySetOps(val mutables: T) extends AnyVal {

    def setMutability(graph : DependencyGraph, name : Seq[String], m : Mutability) : T =
      MutabilitySet.setMutableWithName(graph, mutables, graph.rootId, name, m)

    def setMutability(id : NodeId, v : Mutability) : T = v match {
      case Immutable => mutables - id
      case Mutable => mutables + id
    }

    def setMutability(ids : Iterable[NodeId], v : Mutability) : T = v match {
      case Immutable => mutables -- ids
      case Mutable => mutables ++ ids
    }

    def mutableNodes(graph: DependencyGraph) : T = mutables
    def immutableNodes(graph: DependencyGraph) : T = graph.nodesId.toSet -- mutables

    def toggle(id : NodeId): T =
      setMutability(id, mutability(id).opposite)


    def isImmutable : NodeId => Boolean = id => !isMutable(id)
    def isMutable : NodeId => Boolean = mutables.contains

    def mutability(id : NodeId) : Mutability =
      if(isImmutable(id)) Immutable
      else Mutable

  }

}