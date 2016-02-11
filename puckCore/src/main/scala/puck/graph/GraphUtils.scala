package puck.graph

import puck.graph.io.{DG2ASTBuilder, DotHelper}
import puck.graph.transformations.TransformationRules

trait GraphUtils {
  val dg2astBuilder : DG2ASTBuilder
  val nodeKindKnowledge : NodeKindKnowledge
  val transformationRules : TransformationRules
  val violationsKindPriority : Seq[NodeKind]
  val dotHelper : DotHelper
}
