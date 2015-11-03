package puck

import org.scalatest.OptionValues
import puck.graph._


trait GetDefinitionValue {
  self : OptionValues =>
  def getDefinition(g : DependencyGraph, nid : NodeId) : NodeId =
    g.getConcreteNode(nid).definition(g).value
}
