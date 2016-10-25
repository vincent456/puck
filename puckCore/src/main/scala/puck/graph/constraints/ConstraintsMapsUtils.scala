package puck.graph.constraints

import puck.graph.{DependencyGraph, NodeId}

/**
  * Created by Loïc Girault on 05/10/16.
  */
object ConstraintsMapsUtils {

  def nodesIn(graph: DependencyGraph, r : Range) : Set[NodeId] =
    r match {
      case Element(id) => Set(id)
      case Scope(root) => (graph subTree root).toSet
    }

  def nodesIn(graph: DependencyGraph, rs : RangeSet,
              acc0 : Set[NodeId]) : Set[NodeId] =
    rs.iterator.foldLeft(acc0){
      case (acc, r) => acc ++ nodesIn(graph, r)
    }

/*
  def nodesIn(graph: DependencyGraph, ct : Constraint,
              acc0 : Set[NodeId]) : Set[NodeId] = {
    val s1 = nodesIn(graph, ct.owners, acc0)
    val s2 = nodesIn(graph, ct.facades, s1)
    val s3 = nodesIn(graph, ct.interlopers, s2)
    nodesIn(graph, ct.friends, s3)
  }
*/
// hide A except B from C but-no-from D => here we collect the nodes in A \ B
  def nodesIn(graph: DependencyGraph, ct : Constraint,
              acc0 : Set[NodeId]) : Set[NodeId] =
    acc0 ++ (nodesIn(graph, ct.owners, Set[NodeId]()) --nodesIn(graph, ct.facades, Set[NodeId]()) )


  def nodesIn(graph: DependencyGraph,
              cm : Map[Range, ConstraintSet],
              acc0 : Set[NodeId] = Set[NodeId]()): Set[NodeId] = {
    cm.values.foldLeft(acc0){
      case (acc1, constraintSet) =>
        constraintSet.content.foldLeft(acc1) {
          case (acc2, ct) => nodesIn(graph, ct, acc2)
        }
    }
  }

  def nodesIn(graph: DependencyGraph, cm : ConstraintsMaps): Set[NodeId] = {
    val s1 = nodesIn(graph, cm.friendConstraints)
    nodesIn(graph, cm.hideConstraints, s1)
  }

  implicit class GraphOps(val g : DependencyGraph) extends AnyVal {

    def nodesIn(cm : ConstraintsMaps): Set[NodeId] =
      ConstraintsMapsUtils.nodesIn(g, cm)

  }

}
