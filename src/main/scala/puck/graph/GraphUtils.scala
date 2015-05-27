package puck.graph

import puck.graph.io.DG2ASTBuilder
import puck.graph.transformations.TransformationRules

trait GraphUtils {
  val transformationRules : TransformationRules
  val violationsKindPriority : Seq[NodeKind]
  val dG2ASTBuilder: DG2ASTBuilder
}
