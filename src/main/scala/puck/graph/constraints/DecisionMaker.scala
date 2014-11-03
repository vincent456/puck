package puck.graph.constraints

import puck.graph.{NodeId, AccessGraph, NodeKind}


/**
 * Created by lorilan on 28/05/14.
 */

trait DecisionMaker[Kind <: NodeKind[Kind], T]{
  type NIdT = NodeId[Kind]
  type GraphT = AccessGraph[Kind, T]
  type PredicateT = (GraphT, NIdT) => Boolean

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
