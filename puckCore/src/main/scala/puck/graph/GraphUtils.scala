package puck.graph

import puck.DG2ASTBuilder
import puck.graph.io.DotHelper
import puck.graph.transformations.TransformationRules

trait GraphUtils {
  val dg2astBuilder : DG2ASTBuilder
  val nodeKindKnowledge : NodeKindKnowledge
  val transformationRules : TransformationRules
  val violationsKindPriority : Seq[NodeKind]
  val dotHelper : DotHelper
}
