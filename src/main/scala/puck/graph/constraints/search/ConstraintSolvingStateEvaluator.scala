package puck.graph.constraints.search

import puck.graph._
import puck.graph.immutable.AccessGraph
import puck.search.{SearchState, Evaluator}

/**
 * Created by lorilan on 07/11/14.
 */
class ConstraintSolvingStateEvaluator
(val initialRecord : Recording)
  extends Evaluator[ResultT]{

  def evaluate(s : SearchState[ResultT]): Double ={
    graphOfResult(s.result).coupling
  }

  def equals(s1 : SearchState[ResultT], s2 : SearchState[ResultT] ): Boolean = {
    AccessGraph.areEquivalent(initialRecord, graphOfResult(s1.result), graphOfResult(s2.result))
  }

}
