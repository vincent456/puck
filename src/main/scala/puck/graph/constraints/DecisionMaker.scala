package puck.graph.constraints

import puck.graph._


/**
 * Created by lorilan on 28/05/14.
 */

case class NoAbstractionKindFound(implKind : NodeKind) extends Throwable

trait DecisionMaker{
  type NIdT = NodeId
  type GraphT = DependencyGraph
  type PredicateT = (GraphT, ConcreteNode) => Boolean
  type ResT = ResultT

  def violationTarget(graph : GraphT)
                     (k: Option[ConcreteNode] => Unit) : Unit

  def abstractionKindAndPolicy(graph : GraphT, impl : ConcreteNode)
                              (k : Option[(NodeKind, AbstractionPolicy)] => Unit) : Unit

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : GraphT => Option[NIdT] => Unit) : Unit

/*  def modifyConstraints(graph : GraphT,
                        sources : NodeSet[Kind],
                        target : NodeType) : GraphT*/

}
