package puck.graph.constraints

import puck.graph.{ResultT, NodeId, DependencyGraph, NodeKind}


/**
 * Created by lorilan on 28/05/14.
 */

case class NoAbstractionKindFound(implKind : NodeKind) extends Throwable

trait DecisionMaker{
  type NIdT = NodeId
  type GraphT = DependencyGraph
  type PredicateT = (GraphT, NIdT) => Boolean
  type ResT = ResultT

  def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Unit) : Unit

  def abstractionKindAndPolicy(graph : GraphT, impl : NIdT)
                              (k : Option[(NodeKind, AbstractionPolicy)] => Unit) : Unit

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : Option[NIdT] => Unit) : Unit

/*  def modifyConstraints(graph : GraphT,
                        sources : NodeSet[Kind],
                        target : NodeType) : GraphT*/

}
