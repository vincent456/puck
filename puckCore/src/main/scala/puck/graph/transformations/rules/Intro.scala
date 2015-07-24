package puck.graph.transformations.rules

import puck.graph._

abstract class Intro {

  def apply // real intro, other methods are implementation of the abstract rule
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind,
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) =
    graph.addConcreteNode(localName, kind, mutable)

  def accessToType
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typeNode : NodeId,
   mutable : Mutability = true
    )  : (ConcreteNode, DependencyGraph)
}
