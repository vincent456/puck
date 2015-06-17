package puck.graph.transformations.rules

import puck.graph.{NodeId, DependencyGraph}

class Renamer {
  def apply
  ( g : DependencyGraph,
    id : NodeId,
    newName : String) : DependencyGraph = {
    g.setName(id, newName)
  }
}
