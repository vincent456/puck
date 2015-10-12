package puck.graph.constraints.search

import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.graph.{DependencyGraph, NodeId}

import scala.util.Random


object ConstraintSolvingNodesChoice {

  def apply(k : ChooseNodeKArg => Unit,
            cs : Seq[Option[(DependencyGraph, NodeId)]]) =
    build(k, cs.toSet)

  def includeNoneChoice(k : ChooseNodeKArg => Unit,
                        cs : Seq[Option[(DependencyGraph, NodeId)]])= {
    build(k, cs.toSet + None)
  }

  def build(k : ChooseNodeKArg => Unit, choices : Set[Option[(DependencyGraph, NodeId)]]) =
    new ConstraintSolvingChoice[Option[(DependencyGraph, NodeId)]](k, Random.shuffle(choices), Set[Option[(DependencyGraph, NodeId)]]())
}




