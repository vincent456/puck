package puck.graph

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.rules.Intro

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

  def kindType : KindType
}

object KindType {
  val isStatic : KindType => Boolean = {
    case NameSpace => true
    case TypeDecl => true
    case StaticValueDecl => true
    case InstanceTypeDecl => false
    case InstanceValueDecl => false
    case TypeConstructor => true
    case Parameter => ???
    case ValueDef => ???
    case UnknownKindType => ???
  }
  def isInstance(kt : KindType) : Boolean = !isStatic(kt)
}

sealed trait KindType
case object UnknownKindType extends KindType
case object NameSpace extends KindType

case object TypeDecl extends KindType
case object InstanceTypeDecl extends KindType

case object TypeConstructor extends KindType

case object InstanceValueDecl extends KindType
case object StaticValueDecl extends KindType

case object Parameter extends KindType
case object ValueDef extends KindType

trait AGRoot extends NodeKind {
  def canContain(k: NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("Root node cannot be abstracted")

  def kindType = NameSpace
}

trait NodeKindKnowledge {

  def rootKind : NodeKind

  def nodeKinds : Seq[NodeKind]

  def lightKind : NodeKind

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

  def writeType(graph : DependencyGraph) : Type

  def defaultKindForNewReceiver : NodeKind

  def intro : Intro

  def getConstructorOfType(g: DependencyGraph, tid : NodeId) : Option[NodeId]

}