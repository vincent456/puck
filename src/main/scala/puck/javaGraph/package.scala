package puck

import java.io.File

import puck.graph.{NodeKind, GraphUtils}
import puck.graph.io.{DG2ASTBuilder, FilesHandler}
import puck.graph.transformations.TransformationRules
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.{JavaIntro, JavaRenamer, JavaAbstract, JavaTransformationHelper}

package object javaGraph {
  val defaultPackageName = "<default package>"

  def filterPackageName(name: String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

  def JavaFilesHandler() : FilesHandler = JavaFilesHandler(new File("."))
  def JavaFilesHandler(workingDirectory : java.io.File) : FilesHandler =
      new graph.FilesHandler(workingDirectory,
       ".java",
        JavaDotHelper)
  object JGraphUtils extends GraphUtils {

    val transformationRules: TransformationRules =
      new TransformationRules(JavaTransformationHelper, JavaRenamer, JavaAbstract, JavaIntro)

    val dG2ASTBuilder: DG2ASTBuilder = JavaDG2AST

    val violationsKindPriority: Seq[NodeKind] =
      Seq[JavaNodeKind]( Field, Constructor, Class, Interface)
  }

}
