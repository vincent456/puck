package puck.search

import java.io.{File, FileWriter}

import puck.graph._
import puck.Quick
import puck.graph.constraints.ConstraintsMaps
import puck.graph.DecoratedGraphOps
import puck.graph.io.DotHelper
import puck.util.LoggedEither

import scalaz.\/-

/**
  * Created by cedric on 18/05/2016.
  */
object AStarSearchStrategyGraphDisplay {
  def initDir(name: String): Unit = {
    val dir = new File(name)

    if (!dir.exists())
      dir.mkdirs()
    else {
      val files: Array[String] = dir.list()
      files foreach {
        case f: String =>
          //println(f)
          import puck.util.FileHelper.FileOps
          (dir \ f).delete()
      }
      dir.mkdirs()
    }
  }


}

class AStarSearchStrategyGraphDisplay[T]
(evaluator: Evaluator [DecoratedGraph[T]],
 scm : Option[ConstraintsMaps] = None,
 maxDepth : Int = 100, // ajouté par Mikal
 maxSize : Int = 10,
 dir : String)(implicit dotHelper : DotHelper)
  extends AStarSearchStrategy[DecoratedGraph[T]](evaluator, maxDepth, maxSize) {

  AStarSearchStrategyGraphDisplay.initDir(dir)

  var i : Integer = 0

  override def addState(s: SearchState[DecoratedGraph[T]]): Unit = {
    super.addState(s)
    if (isSuccess(s)) {
      // Quick.frame(dg.graph, "Best current solution", scm)
      //     println ("size = " + remainingStates.length +" "+  remainingStates.mkString("/") )
      remainingStates foreach {
        rs =>
          val \/-(dg) = rs.loggedResult.value
          val v = Metrics.numViolations(dg.graph, scm.get)
         print(s"$rs (${SearchStateOrdering.evaluateWithDepthPenalty(rs)}, ${v}V) / ")
      }
      println()

      if(remainingStates.nonEmpty) {
        i+=1
        val name =  "#_"+ i + "_#" + remainingStates.head.toString
        printSuccessState(name, remainingStates.head)
      }
    }
  }

  def printSuccessState(name : String,
                        ss : SearchState[DecoratedGraph[T]]) : Unit = {
    val LoggedEither(log, \/-(dg)) = ss.loggedResult
    Quick.svg(dg.graph, dir + File.separator + name + ".svg", scm)
    val fw = new FileWriter(dir + File.separator + name + ".transfos")
    fw.write(log)
    fw.close()
  }
}

class AStarSearchStrategyGraphDisplayOnly[T]
(evaluator: Evaluator [DecoratedGraph[T]],
 scm : Option[ConstraintsMaps] = None,
 maxDepth : Int = 100, // ajouté par Mikal
 maxSize : Int = 10,
 dir : String)
  (implicit dotHelper : DotHelper)
  extends AStarSearchStrategy[DecoratedGraph[T]](evaluator, maxDepth, maxSize) {

  AStarSearchStrategyGraphDisplay.initDir(dir)

  var i : Integer = 0

  override def addState(s: SearchState[DecoratedGraph[T]]): Unit = {
    super.addState(s)
    if (isSuccess(s)) {
      // Quick.frame(dg.graph, "Best current solution", scm)
      //     println ("size = " + remainingStates.length +" "+  remainingStates.mkString("/") )
      remainingStates foreach {
        rs =>
          val \/-(dg) = rs.loggedResult.value
          val v = Metrics.numViolations(dg.graph, scm.get)
          print(s"$rs (${SearchStateOrdering.evaluateWithDepthPenalty(rs)}, ${v}V) / ")
      }
      println()

      if(remainingStates.nonEmpty) {
        i+=1
        val name =  "#_"+ i + "_#" + remainingStates.head.toString
        //printSuccessState(name, remainingStates.head)
      }
    }
  }

  def printSuccessState(name : String,
                        ss : SearchState[DecoratedGraph[T]]) : Unit = {
    val LoggedEither(log, \/-(dg)) = ss.loggedResult
    Quick.svg(dg.graph, dir + File.separator + name + ".svg", scm)
    val fw = new FileWriter(dir + File.separator + name + ".transfos")
    fw.write(log)
    fw.close()
  }
}
