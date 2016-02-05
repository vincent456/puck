package puck.graph

import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.rules.Intro

trait NodeKind {
  def canContain(k : NodeKind) : Boolean
  def canBe(k : NodeKind) : Boolean = false
  def abstractionPolicies : Seq[AbstractionPolicy] =
    Seq(SupertypeAbstraction, DelegationAbstraction)

  def isWritable : Boolean = false

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
    //case InstanceTypeDecl => false
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
//case object InstanceTypeDecl extends KindType

case object TypeConstructor extends KindType

case object InstanceValueDecl extends KindType
case object StaticValueDecl extends KindType

case object Parameter extends KindType
case object ValueDef extends KindType

object Role{
  def isFactory(dg : DependencyGraph, cn : ConcreteNode) : Boolean =
    dg.getRole(cn.id) match {
      case Some(Factory(_)) => true
      case _ => false
    }

  def isInitializer(dg : DependencyGraph, cn : ConcreteNode) : Boolean =
    dg.getRole(cn.id) match {
      case Some(Initializer(_)) => true
      case _ => false
    }
}

sealed abstract class Role
case class Initializer(typeDecl : NodeId) extends Role
case class Factory(constructor: NodeId) extends Role
//case class Getter(field : NodeId) extends Role
//case class Setter(field : NodeId) extends Role


trait AGRoot extends NodeKind {
  def canContain(k: NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) =
    throw new DGError("Root node cannot be abstracted")

  def kindType = NameSpace
}

trait NodeKindKnowledge {

  def root : ConcreteNode

  def nodeKinds : List[NodeKind]

  def lightKind : NodeKind

  def canContain
  ( graph : DependencyGraph,
    n : DGNode,
    other : ConcreteNode) : Boolean =
    !graph.contains_*(other.id, n.id) && // no cycle !
      canContain(graph, n, other.kind)


  def canContain
  ( graph : DependencyGraph,
    n : DGNode,
    otherKind : NodeKind) : Boolean =
    (n.kind canContain otherKind) &&
      n.mutable



  def kindOfKindType(kindType: KindType) : Seq[NodeKind]

  def canBe(graph : DependencyGraph)
            (n : DGNode, other : ConcreteNode) : Boolean = {
    !graph.isa_*(other.id, n.id) && // no cycle !
      (n.kind canBe other.kind) &&
      n.mutable
  }

  def writeType(graph : DependencyGraph) : Type

  def defaultKindForNewReceiver : NodeKind

  def initializerKind : NodeKind

  def intro : Intro

  def getConstructorOfType(g: DependencyGraph, tid : NodeId) : Option[NodeId]

  def structuredType(graph : DependencyGraph, id : NodeId, params : List[NodeId]) : Option[Type] = {
    //assert node is a typed value
    if(params.isEmpty) graph styp id
    else Some(Arrow(Tuple(params map (pid => graph styp pid get)), graph styp id get))
  }
  
  

}