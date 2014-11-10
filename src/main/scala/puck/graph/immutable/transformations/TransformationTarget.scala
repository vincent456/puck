package puck.graph.immutable.transformations

import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable.AccessGraph._
import puck.graph.immutable._

/**
 * Created by lorilan on 05/11/14.
 */
sealed abstract class TransformationTarget{
  type GraphT= AccessGraph
  type STyp = TypeHolder
  def execute(g: GraphT, op : Operation) : GraphT
}

case class TTNode
( id : NodeId,
  name : String,
  kind : NodeKind,
  styp : TypeHolder,
  mutable : Boolean,
  t : Hook)
  extends TransformationTarget{

  def execute(g: GraphT, op : Operation) = op match {
    case Add => g.addNode(id, name, kind, styp, mutable, t)
    case Remove => g.removeNode(id)
  }
}
case class TTEdge(edge : AGEdge)
  extends TransformationTarget{

  def execute(g: GraphT, op : Operation) = op match {
    case Add =>edge.create(g)
    case Remove => edge.delete(g)
  }
}

sealed abstract class Extremity{
  val node : NodeId
  def create(n : NodeId) : Extremity
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

case class TTRedirection(edge : AGEdge, extremity : Extremity)
  extends TransformationTarget{

  def execute(g: GraphT, op : Operation) = (op, extremity) match {
    case (Add, Target(newTarget)) => edge.changeTarget(g, newTarget)
    case (Remove, Target(newTarget)) => AGEdge(edge.kind, edge.source, newTarget).changeTarget(g, edge.target)
    case (Add, Source(newSource)) => edge.changeSource(g, newSource)
    case (Remove,Source(newSource)) => AGEdge(edge.kind, newSource, edge.target).changeSource(g, edge.source)
  }
}

class RedirectionWithMerge(edge : AGEdge, extremity : Extremity)
  extends TTRedirection(edge, extremity){

  override def execute(g: GraphT, op : Operation) = (op, extremity) match {
    case (Add, _) => super.execute(g, op)
    case (Remove, _) => edge.create(g)
  }

}

case class TTTypeRedirection
(typed : NodeId,
 typ : TypeHolder,
 oldUsee: NodeId,
 newUsee : NodeId)
  extends TransformationTarget{

  override def execute(g: AccessGraph, op: Operation) = op match {
    case Add => g.changeType(typed, typ, oldUsee, newUsee)
    case Remove => g.changeType(typed, typ, newUsee, oldUsee)
  }
}

case class TTAbstraction
(impl: NodeId,
 abs: NodeId,
 policy: AbstractionPolicy)
 extends TransformationTarget{

  def execute(g: GraphT, op : Operation) = op match {
    case Add => g.addAbstraction(impl, (abs, policy))
    case Remove => g.removeAbstraction(impl, (abs, policy))
  }
}

/*
case class TTDependency(dominant : AGEdge,
                                                   dominated : AGEdge)
  extends TransformationTarget[Kind,T]{

  def execute(g: GraphT, op : Operation) = ???
}




case class TTConstraint(ct : Constraint,
                                                friend : AGNode)
  extends TransformationTarget{

  def execute(op : Operation) = ???
}*/