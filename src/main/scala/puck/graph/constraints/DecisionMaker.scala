package puck.graph.constraints

import puck.graph._
import puck.graph.transformations.rules.CreateVarStrategy


/**
 * Created by lorilan on 28/05/14.
 */

trait NodePredicate {
  def apply(dg : DependencyGraph, cn : ConcreteNode) : Boolean
  override def toString : String = "NodePredicate"
}

trait DecisionMaker{

  def violationTarget(graph : DependencyGraph)
                     (k: Option[ConcreteNode] => Unit) : Unit

  def abstractionKindAndPolicy(graph : DependencyGraph, impl : ConcreteNode)
                              (k : Option[(NodeKind, AbstractionPolicy)] => Unit) : Unit

  def chooseNode(graph : DependencyGraph, predicate : NodePredicate)
                (k : DependencyGraph => Option[NodeId] => Unit) : Unit

  def chooseContainerKind(graph : DependencyGraph, toBeContained : DGNode)
                         (k : Option[NodeKind] => Unit) : Unit

  def createVarStrategy(k : CreateVarStrategy => Unit) : Unit

/*  def modifyConstraints(graph : GraphT,
                        sources : NodeSet[Kind],
                        target : NodeType) : GraphT*/

}
