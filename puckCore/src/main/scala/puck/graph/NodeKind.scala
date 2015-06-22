package puck.graph

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}

trait NodeKind {
  def canContain(k : NodeKind) : Boolean
  def canBe(k : NodeKind) : Boolean = false
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)

  def canBeReadOrWrote : Boolean = false

  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind]

  def canBeAbstractedWith(p: AbstractionPolicy) =
    abstractionNodeKinds(p).nonEmpty

  def abstractionChoices : Seq[(NodeKind, AbstractionPolicy)] =
    for {
      p <- abstractionPolicies
      k <- abstractionNodeKinds(p)
    } yield (k, p)
}

sealed trait KindType
case object Unknown extends KindType
case object NameSpace extends KindType
case object TypeDecl extends KindType
case object TypeMember extends KindType
case object TypeConstructor extends KindType

case object TypeDeclAndTypeMember extends KindType

trait AGRoot extends NodeKind {
  def canContain(k: NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("Root node cannot be abstracted")
}

trait NodeKindKnowledge {

  def rootKind : NodeKind

  def nodeKinds : Seq[NodeKind]
  def canContain(graph : DependencyGraph)
                (n : DGNode, other : ConcreteNode) : Boolean = {
    !graph.contains_*(other.id, n.id) && // no cycle !
      (n.kind canContain other.kind) &&
      n.mutable
  }

  def kindOfKindType(kindType: KindType) : Seq[NodeKind]

  def canBe(graph : DependencyGraph)
            (n : DGNode, other : ConcreteNode) : Boolean = {
    !graph.isa_*(other.id, n.id) && // no cycle !
      (n.kind canBe other.kind) &&
      n.mutable
  }


  def kindType : (DependencyGraph, DGNode) => KindType = (_,_) => Unknown

  //TODO?? move elsewhere ?
  def coupling(graph : DependencyGraph) =
    graph.concreteNodesId.foldLeft(0 : Double){
    (acc, id) => acc + Metrics.coupling(id, graph)
  }

}