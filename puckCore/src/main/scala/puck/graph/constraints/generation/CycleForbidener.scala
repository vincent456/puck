package puck.graph.constraints.generation

import puck.graph.constraints.{LiteralRangeSet, Scope, Constraint, ConstraintsMaps}
import puck.graph.{NodeIdP, NameSpace, NodeId, DependencyGraph}
import puck.util.GreedyCycleRemover.WDGHelper
import puck.util.{GreedyCycleRemover, WeightedDirectedGraph}

/**
  * Created by lorilan on 10/12/15.
  */
object CycleForbidener {
  val isNameSpace : (DependencyGraph, NodeId) => Boolean = {
    _.getNode(_).kind.kindType == NameSpace
  }

  def edgeToConstraint(e : NodeIdP) : Constraint = e match {
    case (user, used) =>
      val emptySet = LiteralRangeSet.empty
      val interloper = LiteralRangeSet(Scope(user))
      val owner = LiteralRangeSet(Scope(used))
      new Constraint(owner, emptySet, interloper, emptySet)
  }


  def genConstraints(g : DependencyGraph) : DependencyGraph ={
    val wdg = WeightedDirectedGraph.fromDG(g, isNameSpace)
    val remover = new GreedyCycleRemover(WDGHelper)
    val edgesToRemove = remover.edgesToRemove(wdg)
    val cm =
      (edgesToRemove map edgeToConstraint).
        foldLeft(ConstraintsMaps())(_.addHideConstraint(_))
    g.newGraph(constraints = cm)
  }
}
