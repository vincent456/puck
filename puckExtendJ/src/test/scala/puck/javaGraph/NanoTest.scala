package puck.javaGraph

import java.io.File

import puck.util.{PuckFileLogger, PuckLogger}
import puck._
import puck.graph.constraints.search.{NoVirtualNodes, WithVirtualNodes}

/**
  * Created by cedric on 22/09/2016.
  */
object NanoTest {

  implicit val logger : PuckLogger = new PuckFileLogger(_ => true,
    new File("/tmp/pucklog"))

  val root = getClass.getResource("/nanoPersonne/").getPath

  println(root)

  val project = Tests.project(root, "nano", "decouple.wld")

  def main(args : Array[String]) : Unit = {
    val dg2ast = project.loadGraph()
    val scm = project.parseConstraints(dg2ast)

    SearchSolution(project, ".",
      StrategyChoice.DepthFirst, ControlChoice.Heuristic, NoVirtualNodes)

    //    ignore(applyRecords(project,
    //      Seq(root + path +"/heuristic-depthFirst-solution-0_6_0.pck" )))
  }
}
