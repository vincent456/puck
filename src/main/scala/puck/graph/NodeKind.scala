package puck.graph

import puck.graph.constraints.{SupertypeAbstraction, DelegationAbstraction, AbstractionPolicy}

/**
 * Created by lorilan on 26/10/14.
 */
trait NodeKind {
  def canContain(k : NodeKind) : Boolean
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)
  def abstractKinds(p : AbstractionPolicy) : Seq[NodeKind]

  def isTypeDecl : Boolean = false
  def isTypeMember : Boolean = false
  def isTypeConstructor : Boolean = false

}
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
  def isTypeUse : DependencyGraph => DGEdge => Boolean
  def isTypeMemberUse : DependencyGraph => DGEdge => Boolean

  //TODO?? move elsewhere ?
  def coupling(graph : DependencyGraph) =
    graph.concreteNodesId.foldLeft(0 : Double){
    (acc, id) => acc + Metrics.coupling(id, graph)
  }

}