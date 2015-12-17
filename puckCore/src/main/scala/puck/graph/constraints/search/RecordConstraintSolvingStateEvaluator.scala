package puck.graph.constraints.search

import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.transformations.Transformation
import puck.search.{Evaluator, SearchState}

import scalaz.{-\/, \/-}

class RecordConstraintSolvingStateEvaluator
(val initialRecord : Seq[Transformation])
  extends Evaluator[SResult]{


  def evaluate(s : SearchState[SResult]): Double =
    s.loggedResult.value match {
      case -\/(err) => 0
      case \/-(res) =>
        val g = graphOfResult(res)
        Metrics.nameSpaceCoupling(g)
    }

  def equals(s1 : SearchState[SResult], s2 : SearchState[SResult] ): Boolean =
    (s1.loggedResult.value, s2.loggedResult.value) match {
      case (\/-(res1), \/-(res2)) =>
        DependencyGraph.areEquivalent(initialRecord,
          graphOfResult(res1),
          graphOfResult(res2))
      case _ => false
    }



}


object GraphConstraintSolvingStateEvaluator
  extends Evaluator[SResult]{

  def evaluate(s : SearchState[SResult]): Double =
    s.loggedResult.value match {
      case -\/(err) => 0
      case \/-(res) =>
        val g = graphOfResult(res)
        Metrics.nameSpaceCoupling(g)
    }

  def equals(s1 : SearchState[SResult], s2 : SearchState[SResult] ): Boolean =
    (s1.loggedResult.value, s2.loggedResult.value) match {
      case (\/-(res1), \/-(res2)) =>
        Mapping.equals(res1._1, res2._1)
      case _ => false
    }



}

