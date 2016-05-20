package puck.search

import java.io.{File, FileOutputStream}

import puck.graph._
import puck.Quick
import puck.graph.constraints.ConstraintsMaps
import puck.graph.DecoratedGraphOps
import puck.graph.io.{DotHelper, DotPrinter, Svg}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scalaz.\/-

/**
  * Created by cedric on 18/05/2016.
  */
class AStarSearchStrategyGraphDisplay[T]
(evaluator: Evaluator [DecoratedGraph[T]],
 scm : Option[ConstraintsMaps] = None,
 maxDepth : Int = 100, // ajouté par Mikal
 maxSize : Int = 10,
 tmpDir : String)(implicit dotHelper : DotHelper)
  extends AStarSearchStrategy[DecoratedGraph[T]](evaluator, maxDepth, maxSize) {

  val dir = new File(tmpDir+"DG-Imgs")
  var i : Integer = 0
  if(!dir.exists())
    dir.mkdirs()

  override def addState(s: SearchState[DecoratedGraph[T]]): Unit = {
    super.addState(s)
    if (isSuccess(s)) {
      // Quick.frame(dg.graph, "Best current solution", scm)
      //     println ("size = " + remainingStates.length +" "+  remainingStates.mkString("/") )
      remainingStates foreach {
        rs =>
          val \/-(dg) = rs.loggedResult.value
         print(s"$rs (${SearchStateOrdering.evaluateWithDepthPenaly(rs)}, ${Metrics.numViolations(dg.graph, scm.get)}V) / ")
      }
      println()

      if(remainingStates.nonEmpty) {
        i+=1
        val \/-(dg) = remainingStates.head.loggedResult.value
        Quick.svg(dg.graph, dir.getCanonicalPath + File.separator + "#_"+ i + "_#" + remainingStates.head.toString + ".svg", scm)
      }
    }
  }
}
