package puck.graph.transformations

import puck.graph._
import puck.graph.constraints.AbstractionPolicy

sealed trait Operation{

  def execute(g: DependencyGraph , op : Direction) : DependencyGraph
  def productPrefix : String
}

case class CNode(n : ConcreteNode) extends Operation {

  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addConcreteNode(n)
    case Reverse => g.removeConcreteNode(n)
  }
}


case class VNode(n : VirtualNode) extends Operation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addVirtualNode(n)
    case Reverse => g.removeVirtualNode(n)
  }
}

case class Edge(edge : DGEdge)
  extends Operation {

  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular =>edge.createIn(g)
    case Reverse => edge.deleteIn(g)
  }
}

sealed abstract class Extremity{
  val node : NodeId
  def create(n : NodeId) : Extremity
  def productPrefix : String
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]): AGNode[K]*/
}
case class Source(node : NodeId) extends Extremity{
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]) = e.source*/
  def create(n : NodeId) : Extremity = Source(n)

}
case class Target(node : NodeId) extends Extremity  {
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]) = e.target*/
  def create(n : NodeId) : Extremity = Target(n)
}

case class RedirectionOp(edge : DGEdge, extremity : Extremity)
  extends Operation{

  val withMerge = false
  def execute(g: DependencyGraph , op : Direction) = (op, extremity) match {
    case (Regular, Target(newTarget)) => edge.changeTarget(g, newTarget)
    case (Reverse, Target(newTarget)) => edge.kind(edge.source, newTarget).changeTarget(g, edge.target)
    case (Regular, Source(newSource)) => edge.changeSource(g, newSource)
    case (Reverse,Source(newSource)) => edge.kind(newSource, edge.target).changeSource(g, edge.source)
  }
}

object RedirectionWithMerge {
  def unapply( t : RedirectionWithMerge) : Some[( DGEdge, Extremity)] =
    Some((t.edge, t.extremity))
}

class RedirectionWithMerge(edge : DGEdge, extremity : Extremity)
  extends RedirectionOp(edge, extremity){
  override val productPrefix = "RedirectionWithMerge"

  override def copy(edge : DGEdge = edge, extremity: Extremity = extremity) =
    new RedirectionWithMerge(edge, extremity)

  override val withMerge = true

  override def execute(g: DependencyGraph , op : Direction) = (op, extremity) match {
    case (Regular, _) => super.execute(g, op)
    case (Reverse, _) => edge.createIn(g)
  }

}

case class ChangeNodeName
( nid : NodeId,
  oldName : String,
  newName : String)
extends Operation {
  override def execute(g: DependencyGraph , op: Direction): DependencyGraph  =
  op match {
    case Regular => g.setName(nid, newName)
    case Reverse => g.setName(nid, oldName)
  }
}


case class TypeRedirection
(typed : NodeId,
 styp : Option[Type],
 oldUsee: NodeId,
 newUsee : NodeId)
  extends Operation{

  override def execute(g: DependencyGraph, op: Direction) = op match {
    case Regular => g.changeType(typed, styp, oldUsee, newUsee)
    case Reverse => g.changeType(typed, styp, newUsee, oldUsee)
  }
}

case class Abstraction
(impl: NodeId,
 abs: NodeId,
 policy: AbstractionPolicy)
 extends Operation{

  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addAbstraction(impl, (abs, policy))
    case Reverse => g.removeAbstraction(impl, (abs, policy))
  }
}


case class TypeDependency
( typeUse : NodeIdP,
  typeMemberUse :  NodeIdP)
  extends Operation {
  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addUsesDependency(typeUse, typeMemberUse)
    case Reverse => g.removeUsesDependency(typeUse, typeMemberUse)
  }
}



/*
case class TTConstraint(ct : Constraint,
                                                friend : AGNode)
  extends TransformationTarget{

  def execute(op : Operation) = ???
}*/