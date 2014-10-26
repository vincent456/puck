package puck.graph.constraints.search

import puck.graph.constraints.{DecisionMaker, Solver}
import puck.graph.{AccessGraph, NodeKind}
import puck.util.Logger

/**
 * Created by lorilan on 25/09/14.
 */
trait SolverBuilder[Kind <: NodeKind[Kind]]{
  def apply(graph : AccessGraph[Kind],
            dm : DecisionMaker[Kind]) : Solver[Kind]
}
