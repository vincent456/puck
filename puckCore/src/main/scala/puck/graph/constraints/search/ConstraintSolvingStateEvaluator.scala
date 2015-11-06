package puck.graph
package constraints.search

import puck.graph.transformations.Transformation
import puck.search.{SearchState, Evaluator}

object ConstraintSolvingStateEvaluator

class ConstraintSolvingStateEvaluator
(val initialRecord : Seq[Transformation])
  extends Evaluator[DependencyGraph]{

  def evaluate(s : SearchState[DependencyGraph]): Double ={
    val g = graphOfResult(s.loggedResult.value)
    Metrics.nameSpaceCoupling(g)
  }

  def equals(s1 : SearchState[DependencyGraph], s2 : SearchState[DependencyGraph] ): Boolean =
    DependencyGraph.areEquivalent(initialRecord,
      graphOfResult(s1.loggedResult.value),
      graphOfResult(s2.loggedResult.value))
  

}
