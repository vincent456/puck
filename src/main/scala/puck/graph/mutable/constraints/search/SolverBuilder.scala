package puck.graph.mutable.constraints.search

import puck.graph.mutable.{AccessGraph, NodeKind}
import puck.graph.mutable.constraints.{DecisionMaker, Solver}

/**
 * Created by lorilan on 25/09/14.
 */
trait SolverBuilder[Kind <: NodeKind[Kind]]{
  def apply(graph : AccessGraph[Kind],
            dm : DecisionMaker[Kind]) : Solver[Kind]
}
