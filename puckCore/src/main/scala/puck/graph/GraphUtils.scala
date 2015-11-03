package puck.graph

import puck.graph.io.DotHelper
import puck.graph.transformations.TransformationRules

trait GraphUtils {
  val nodeKindKnowledge : NodeKindKnowledge
  val transformationRules : TransformationRules
  val violationsKindPriority : Seq[NodeKind]
  val dotHelper : DotHelper
}
