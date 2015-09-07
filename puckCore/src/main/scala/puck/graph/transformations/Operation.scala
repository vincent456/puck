package puck.graph.transformations

import puck.graph._

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
}
case class Source(node : NodeId) extends Extremity{
  def create(n : NodeId) : Extremity = Source(n)
}
case class Target(node : NodeId) extends Extremity  {
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


case class TypeChange
(typed : NodeId,
 oldType: Option[Type],
 newType : Option[Type])
  extends Operation{

  override def execute(g: DependencyGraph, op: Direction) = op match {
    case Regular => g.setType(typed, newType)
    case Reverse => g.setType(typed, oldType)
  }
}

case class AbstractionOp
(impl: NodeId,
 abs : Abstraction)
 extends Operation{

  def execute(g: DependencyGraph , op : Direction) = op match {
    case Regular => g.addAbstraction(impl, abs)
    case Reverse => g.removeAbstraction(impl, abs)
  }
}



sealed abstract class BoundPart{
  val edge : NodeIdP
  def create(n : NodeIdP) : BoundPart
  def productPrefix : String
}
case class TypeUse(edge : NodeIdP) extends BoundPart{
  def create(e : NodeIdP) : BoundPart = TypeUse(e)
}
case class InstanceValueUse(edge : NodeIdP) extends BoundPart  {
  def create(e : NodeIdP) : BoundPart = InstanceValueUse(e)
}

case class ChangeTypeBinding(oldBinding : (NodeIdP,NodeIdP), extremity : BoundPart) extends Operation {

   def execute(g: DependencyGraph , op : Direction) = (op, extremity) match {
    case (Regular, TypeUse(tu)) =>
      g.changeTypeUseOfTypeMemberUse(oldBinding._1, tu, oldBinding._2)
    case (Reverse, TypeUse(tu)) =>
      g.changeTypeUseOfTypeMemberUse(tu, oldBinding._1, oldBinding._2)
    case (Regular, InstanceValueUse(tmu)) =>
      g.changeTypeMemberUseOfTypeUse(oldBinding._2, tmu, oldBinding._1)
    case (Reverse,InstanceValueUse(tmu)) =>
      g.changeTypeMemberUseOfTypeUse(tmu, oldBinding._2, oldBinding._1)
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

//case class AddFactoryMethod(constructor : NodeId, factory : ConcreteNode) extends Operation


/*
case class TTConstraint(ct : Constraint,
                                                friend : AGNode)
  extends TransformationTarget{

  def execute(op : Operation) = ???
}*/