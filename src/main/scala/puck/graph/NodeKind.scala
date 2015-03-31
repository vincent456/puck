package puck.graph

import puck.graph.constraints.{SupertypeAbstraction, DelegationAbstraction, AbstractionPolicy}

/**
 * Created by lorilan on 26/10/14.
 */
trait NodeKind {
  def canContain(k : NodeKind) : Boolean
  def canBe(k : NodeKind) : Boolean = false
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)
  def abstractKinds(p : AbstractionPolicy) : Seq[NodeKind]
}

sealed trait KindType
case object Unknown extends KindType
case object NameSpace extends KindType
case object TypeDecl extends KindType
case object TypeMember extends KindType
case object TypeConstructor extends KindType

case object TypeDeclMember extends KindType

trait AGRoot extends NodeKind {
  def canContain(k: NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) =
    throw new DGError("Root node cannot be abstracted")
}

trait NodeKindKnowledge {

  def nodeKinds : Seq[NodeKind]
  def canContain(graph : DependencyGraph)
                (n : DGNode, other : ConcreteNode) : Boolean = {
    !graph.contains_*(other.id, n.id) && // no cycle !
      (n.kind canContain other.kind) &&
      n.isMutable
  }

  def kindType : (DependencyGraph, DGNode) => KindType = (_,_) => Unknown

  //TODO?? move elsewhere ?
  def coupling(graph : DependencyGraph) =
    graph.concreteNodesId.foldLeft(0 : Double){
    (acc, id) => acc + Metrics.coupling(id, graph)
  }

}