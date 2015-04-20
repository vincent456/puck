package puck

import java.io.File

import puck.graph.constraints.search.SolverBuilder
import puck.graph.constraints.{Solver, DecisionMaker}
import puck.graph.io.{FilesHandler, ConstraintSolvingSearchEngineBuilder}
import puck.graph.transformations.TransformationRules
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.{JavaIntro, JavaTransformationHelper}

package object javaGraph {
  val defaultPackageName = "<default package>"

  def filterPackageName(name: String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

  val JavaViolationPrioritySeq =
    Seq[JavaNodeKind]( Field, Constructor, Class, Interface)

  val JavaTransformationRules =
    new TransformationRules( JavaTransformationHelper, JavaIntro )

  object JavaSolverBuilder extends SolverBuilder {
    def apply(decisionMaker : DecisionMaker,
              automaticConstraintLoosening : Boolean) : Solver =
      new Solver(decisionMaker,
        JavaTransformationRules,
        automaticConstraintLoosening)
  }

    val JavaSearchingStrategies: Seq[ConstraintSolvingSearchEngineBuilder] =
      List(JavaTryAllCSSEBuilder,
        JavaFunneledCSSEBuilder,
        //JavaGradedCSSEBuilder,
        JavaFindFirstCSSEBuilder)

    def JavaFilesHandler() : FilesHandler = JavaFilesHandler(new File("."))
    def JavaFilesHandler(workingDirectory : java.io.File) : FilesHandler =
      new graph.FilesHandler(workingDirectory,
        JavaSolverBuilder,
        ".java",
        JavaDotHelper,
        JavaTransformationRules,
        JavaDG2AST,
        JavaSearchingStrategies)


}
