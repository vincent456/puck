package puck

import puck.graph.transformations.TransformationRules
import puck.graph._
import puck.graph.io.{DotHelper, DG2ASTBuilder}
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.{JavaIntro, JavaRenamer, JavaAbstract, JavaTransformationHelper}

package object javaGraph {
  val defaultPackageName = "<default package>"

  def filterPackageName(name: String) = name match {
    case "" => defaultPackageName
    case _ => name
  }


  object JGraphUtils extends GraphUtils {

    val nodeKindKnowledge : NodeKindKnowledge = JavaNodeKind

    val transformationRules: TransformationRules =
      new TransformationRules(JavaTransformationHelper, JavaRenamer, JavaAbstract, JavaIntro)

    val violationsKindPriority: Seq[NodeKind] =
      Seq[JavaNodeKind]( Field, Constructor, Class, Interface)

    val dotHelper : DotHelper = JavaDotHelper
  }

}
