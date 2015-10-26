package puck.graph

import puck.graph.io.{DotHelper, DG2ASTBuilder}
import puck.graph.transformations.TransformationRules

trait GraphUtils {
  val nodeKindKnowledge : NodeKindKnowledge
  val transformationRules : TransformationRules
  val violationsKindPriority : Seq[NodeKind]
  val dG2ASTBuilder: DG2ASTBuilder
  val dotHelper : DotHelper
}
