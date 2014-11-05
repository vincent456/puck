package puck.graph.constraints

import puck.graph.{ResultT, NodeId, AccessGraph, NodeKind}


/**
 * Created by lorilan on 28/05/14.
 */

case class NoAbstractionKindFound[K <: NodeKind[K]](implKind : K) extends Throwable

trait DecisionMaker[Kind <: NodeKind[Kind], T]{
  type NIdT = NodeId[Kind]
  type GraphT = AccessGraph[Kind, T]
  type PredicateT = (GraphT, NIdT) => Boolean
  type ResT = ResultT[Kind, T]

  def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Unit) : Unit

  def abstractionKindAndPolicy(graph : GraphT, impl : NIdT)
                              (k : Option[(Kind, AbstractionPolicy)] => Unit) : Unit

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : Option[NIdT] => Unit) : Unit

/*  def modifyConstraints(graph : GraphT,
                        sources : NodeSet[Kind],
                        target : NodeType) : GraphT*/

}
