package puck.search

import puck.Quick
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.DecoratedGraphOps
import puck.graph.io.DotHelper
import scala.collection.mutable
import scalaz.\/-

/**
  * Created by cedric on 18/05/2016.
  */
class AStarSearchStrategyGraphDisplay[T]
(evaluator: Evaluator [DecoratedGraph[T]],
 scm : Option[ConstraintsMaps] = None,
 maxDepth : Int = 100, // ajouté par Mikal
 maxSize : Int = 10)(implicit dotHelper : DotHelper)
  extends AStarSearchStrategy[DecoratedGraph[T]](evaluator, maxDepth, maxSize) {

  override def addState(s: SearchState[DecoratedGraph[T]]): Unit = {
    super.addState(s)
    if (isSuccess(s)) {
      val \/-(dg) = s.loggedResult.value
      // Quick.frame(dg.graph, "Best current solution", scm)
      //     println ("size = " + remainingStates.length +" "+  remainingStates.mkString("/") )
      remainingStates foreach {
        rs => print(s"$rs (${SearchStateOrdering.evaluateWithDepthPenaly(rs)}, ${Metrics.numViolations(dg.graph, scm.get)}V) / ")
      }
      println()
    }
  }
}
