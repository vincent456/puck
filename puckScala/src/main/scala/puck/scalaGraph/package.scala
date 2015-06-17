package puck

import java.io.File

import puck.graph.{NodeKind, NodeKindKnowledge, GraphUtils}
import puck.graph.io.{DG2ASTBuilder, FilesHandler}
import puck.graph.transformations.TransformationRules
import puck.scalaGraph.nodeKind.ScalaNodeKind

package object scalaGraph {

  def ScalaFilesHandler() : FilesHandler = ScalaFilesHandler(new File("."))

  def ScalaFilesHandler(workingDirectory : java.io.File) : FilesHandler =
    new FilesHandler(workingDirectory, ".scala", ScalaDotHelper)

  object ScalaGraphUtils extends GraphUtils {

    val nodeKindKnowledge : NodeKindKnowledge = ScalaNodeKind

    lazy val transformationRules: TransformationRules = ???

    val dG2ASTBuilder: DG2ASTBuilder = ScalaDG2AST

    lazy val violationsKindPriority: Seq[NodeKind] = ???
  }
}
