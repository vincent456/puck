package puck.graph.constraints.search

import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.{AccessGraph, NodeKind}

/**
 * Created by lorilan on 25/09/14.
 */
trait SolverBuilder{
  def apply(graph : AccessGraph, dm : DecisionMaker) : Solver
}
