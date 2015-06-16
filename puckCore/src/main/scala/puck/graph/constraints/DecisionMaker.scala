package puck.graph.constraints

import puck.graph._
import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.graph.transformations.rules.CreateVarStrategy
import puck.util.Logged


trait NodePredicate {
  def apply(dg : DependencyGraph, cn : ConcreteNode) : Boolean
  override def toString : String = "NodePredicate"
}

object DecisionMaker {
  type ChooseNodeKArg = Logged[Option[(DependencyGraph,NodeId)]]
}
trait DecisionMaker{

  def violationTarget
  ( lg : LoggedG)
  ( k: Logged[Option[ConcreteNode]] => Unit) : Unit

  def abstractionKindAndPolicy
  ( lg : LoggedG, impl : ConcreteNode)
  ( k : Logged[Option[(NodeKind, AbstractionPolicy)]] => Unit) : Unit

  def chooseNode
  ( lg : LoggedG, predicate : NodePredicate)
  ( k : ChooseNodeKArg => Unit) : Unit

  def chooseContainerKind
  ( lg : LoggedG, toBeContained : DGNode)
  ( k : Logged[Option[NodeKind]] => Unit) : Unit

  def selectExistingAbstraction
  ( lg : LoggedG, choices : Set[Abstraction])
  ( k : Logged[Option[Abstraction]] => Unit) : Unit

  def createVarStrategy(k : CreateVarStrategy => Unit) : Unit

/*  def modifyConstraints(graph : GraphT,
                        sources : NodeSet[Kind],
                        target : NodeType) : GraphT*/

}
