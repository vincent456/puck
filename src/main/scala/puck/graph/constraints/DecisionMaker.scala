package puck.graph.constraints

import puck.graph.{ResultT, NodeId, AccessGraph, NodeKind}


/**
 * Created by lorilan on 28/05/14.
 */

trait DecisionMaker[Kind <: NodeKind[Kind], T]{
  type NIdT = NodeId[Kind]
  type GraphT = AccessGraph[Kind, T]
  type PredicateT = (GraphT, NIdT) => Boolean
  type ResT = ResultT[Kind, T]

  def violationTarget(graph : GraphT)
                     (k: Option[NIdT] => Option[ResT]) : Option[ResT]

  def abstractionKindAndPolicy(graph : GraphT, impl : NIdT)
                              (k : Option[(Kind, AbstractionPolicy)] => Option[ResT]) : Option[ResT]

  def chooseNode(graph : GraphT, predicate : PredicateT)
                (k : Option[NIdT] => Option[ResT]) : Option[ResT]

/*  def modifyConstraints(graph : GraphT,
                        sources : NodeSet[Kind],
                        target : NodeType) : GraphT*/

}
