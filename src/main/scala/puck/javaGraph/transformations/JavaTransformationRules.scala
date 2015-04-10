package puck.javaGraph.transformations

import puck.graph.transformations.TransformationRules


object JavaTransformationRules
  extends TransformationRules(
    JavaTransformationHelper,
    JavaIntro )
