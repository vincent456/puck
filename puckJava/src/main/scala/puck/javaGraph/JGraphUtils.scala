package puck.javaGraph

import puck.graph.io.DotHelper
import puck.graph.transformations.TransformationRules
import puck.graph.{NodeKind, NodeKindKnowledge, GraphUtils}
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.{JavaIntro, JavaAbstract, JavaRenamer, JavaTransformationHelper}

/**
  * Created by lorilan on 2/21/16.
  */
abstract class JGraphUtils extends GraphUtils {

  val nodeKindKnowledge : NodeKindKnowledge = JavaNodeKind

  val transformationRules: TransformationRules =
    new TransformationRules(JavaTransformationHelper, JavaRenamer, JavaAbstract, JavaIntro)

  val violationsKindPriority: Seq[NodeKind] =
    Seq[JavaNodeKind]( Field, Constructor, Class, Interface)

  val dotHelper : DotHelper = JavaDotHelper

}
