package puck.graph.transformations.rules

import puck.graph._

class Intro {

  def apply // real intro, other methods are implementation of the abstract rule
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind,
    th : Option[Type],
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) =
    graph.addConcreteNode(localName, kind, th, mutable)
}
