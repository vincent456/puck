package puck.graph.constraints.search

import puck.graph.constraints.{Solver, DecisionMaker}

/**
 * Created by lorilan on 25/09/14.
 */
trait SolverBuilder{
  def apply(dm : DecisionMaker, automaticConstraintLoosening : Boolean) : Solver
}
