package puck.graph.constraints.search

import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.{AccessGraph, NodeKind}

/**
 * Created by lorilan on 25/09/14.
 */
trait SolverBuilder[Kind <: NodeKind[Kind], T]{
  def apply(graph : AccessGraph[Kind, T],
            dm : DecisionMaker[Kind, T]) : Solver[Kind, T]
}
