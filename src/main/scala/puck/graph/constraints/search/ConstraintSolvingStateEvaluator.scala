package puck.graph
package constraints.search

import puck.graph.transformations.Transformation
import puck.search.{SearchState, Evaluator}

object ConstraintSolvingStateEvaluator

class ConstraintSolvingStateEvaluator
(val initialRecord : Seq[Transformation])
  extends Evaluator[ResultT]{

  def evaluate(s : SearchState[ResultT]): Double ={
    graphOfResult(s.loggedResult.value).coupling
  }

  def equals(s1 : SearchState[ResultT], s2 : SearchState[ResultT] ): Boolean =
    DependencyGraph.areEquivalent(initialRecord,
      graphOfResult(s1.loggedResult.value),
      graphOfResult(s2.loggedResult.value))
  

}
