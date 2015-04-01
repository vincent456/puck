package puck.javaGraph

import java.io.File

import puck.graph
import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.constraints.search.SolverBuilder
import puck.graph.io.{ConstraintSolvingSearchEngineBuilder, FilesHandler}
import puck.javaGraph.transformations.JavaTransformationRules


/**
 * Created by lorilan on 11/08/14.
 */

object JavaSolverBuilder extends SolverBuilder{
  def apply(dm : DecisionMaker,
            automaticConstraintLoosening : Boolean) : Solver = JavaSolver(dm, automaticConstraintLoosening)
}

object JavaFilesHandler {
  val javaSearchingStrategies: Seq[ConstraintSolvingSearchEngineBuilder] =
    List(JavaTryAllCSSEBuilder,
      JavaFunneledCSSEBuilder,
      //JavaGradedCSSEBuilder,
      JavaFindFirstCSSEBuilder)

  def apply() : FilesHandler = apply(new File("."))
  def apply(workingDirectory : java.io.File) : FilesHandler =
    new graph.FilesHandler(workingDirectory,
      JavaSolverBuilder,
      ".java",
      JavaDotHelper,
      JavaTransformationRules,
      JavaDG2AST,
      javaSearchingStrategies)
}