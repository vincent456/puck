package puck.graph.transformations.rules

import puck.graph._

class Renamer {
  def apply
  ( g : DependencyGraph,
    id : NodeId,
    newName : String) : DependencyGraph = {
    g.setName(id, newName)
  }
}
