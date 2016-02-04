package puck

import puck.graph.{DependencyGraph, ShowDG}
import puck.graph.constraints.generation.CycleForbidener
import puck.graph.constraints.search.{CouplingConstraintSolvingAllViolationsControl, CouplingConstraintSolvingControl}
import puck.jastadd._
import puck.javaGraph.{JavaDotHelper, JGraphUtils}
import puck.search.{SearchEngine, DepthFirstSearchStrategy}
import puck.util.{LoggedEither, PuckLog, PuckSystemLogger}

import scalaz.\/-

/**
  * Created by lorilan on 10/12/15.
  */
object CycleBreaker {
  def main(args: Array[String]) = {
    implicit val logger = new PuckSystemLogger(_ => true)

    val dg2ast = if(args.isEmpty) {
      val fh = JavaFilesHandler()
      fh.loadGraph().asInstanceOf[JavaJastAddDG2AST]
    }
    else JavaJastAddDG2AST.fromFiles(args.toList, List(), List(), logger, null)

    val n = dg2ast.nodesByName("screen.WelcomeCapital.printCapital(String)")

    QuickFrame(dg2ast.initialGraph, "g", JavaDotHelper)

    val searchControlStrategy =
          new CouplingConstraintSolvingControl(
            JGraphUtils.transformationRules, dg2ast.initialGraph,
            dg2ast.initialGraph getConcreteNode n)

    val engine =
         new SearchEngine(new DepthFirstSearchStrategy(), searchControlStrategy)

        engine.explore()
//    val g2 = CycleForbidener.genConstraints(dg2ast.initialGraph)
//    println("#####################")
//    println("Generated constraints :")
//
//    g2.printConstraints(logger, PuckLog.defaultVerbosity)
//
//    import ShowDG._
//    println("#####################")
//
//    val initialViolations = g2.violations()
//    println("Violations :")
//    initialViolations.foreach{
//      e =>
//      dg2ast.graph2ASTMap get e.user match {
//        case Some(n : HasNode) =>
//          print(n.node.compilationUnit().pathName() + ", l "+ n.node.location()+" ")
//        case _ => ()
//      }
//
//      (g2,e).println(fullNameEdgeCord)
//    }



//    val searchControlStrategy =
//      new CouplingConstraintSolvingAllViolationsControl(
//        JGraphUtils.transformationRules, g2,
//        JGraphUtils.violationsKindPriority)
//
//    val engine =
//      new SearchEngine(new DepthFirstSearchStrategy(),
//        searchControlStrategy, Some(1))
//
//    engine.explore()
//
//
//    if(engine.successes.isEmpty)
//      println("no solution found")
//    else {
//      val res =engine.successes.head.loggedResult
//      res match {
//        case LoggedEither(_, \/-(g3)) =>
//          println("applying result")
//          dg2ast.apply(g3)
//        case _ => println("error, should have been a success")
//      }
//    }
  }


}
