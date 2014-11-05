package puck.graph.immutable.transformations

import puck.graph.immutable.AccessGraph._
import puck.graph.immutable.{AccessGraph, AGEdge, TypeHolder, NodeKind}

/**
 * Created by lorilan on 05/11/14.
 */
sealed abstract class TransformationTarget[Kind <: NodeKind[Kind], T]{
  type GraphT= AccessGraph[Kind,T]
  type STyp = TypeHolder[Kind]
  def execute(g: GraphT, op : Operation) : GraphT
}

case class TTNode[Kind <: NodeKind[Kind], T]
( id : NodeId[Kind],
  name : String,
  kind : Kind,
  styp : TypeHolder[Kind],
  mutable : Boolean,
  t : T)
  extends TransformationTarget[Kind, T]{

  def execute(g: GraphT, op : Operation) = op match {
    case Add => g.addNode(id, name, kind, styp, mutable, t)
    case Remove => g.removeNode(id)
  }
}
case class TTEdge[Kind <: NodeKind[Kind], T](edge : AGEdge[Kind])
  extends TransformationTarget[Kind, T]{

  def execute(g: GraphT, op : Operation) = op match {
    case Add =>edge.create[T](g)
    case Remove => edge.delete(g)
  }
}

sealed abstract class Extremity[Kind <: NodeKind[Kind]]{
  val node : NodeId[Kind]
  def create(n : NodeId[Kind]) : Extremity[Kind]
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]): AGNode[K]*/
}
case class Source[Kind <: NodeKind[Kind]](node : NodeId[Kind]) extends Extremity[Kind]{
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]) = e.source*/
  def create(n : NodeId[Kind]) : Extremity[Kind] = Source(n)
}
case class Target[Kind <: NodeKind[Kind]](node : NodeId[Kind]) extends Extremity[Kind]  {
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]) = e.target*/
  def create(n : NodeId[Kind]) : Extremity[Kind] = Target(n)
}

case class TTRedirection[Kind <: NodeKind[Kind], T](edge : AGEdge[Kind],
                                                    extremity : Extremity[Kind])
  extends TransformationTarget[Kind, T]{

  def execute(g: GraphT, op : Operation) = (op, extremity) match {
    case (Add, Target(newTarget)) => edge.changeTarget(g, newTarget)
    case (Remove, Target(newTarget)) => AGEdge(edge.kind, edge.source, newTarget).changeTarget(g, edge.target)
    case (Add, Source(newSource)) => edge.changeSource(g, newSource)
    case (Remove,Source(newSource)) => AGEdge(edge.kind, newSource, edge.target).changeSource(g, edge.source)
  }
}

class RedirectionWithMerge[Kind <: NodeKind[Kind], T](edge : AGEdge[Kind],
                                                      extremity : Extremity[Kind])
  extends TTRedirection[Kind, T](edge, extremity){

  override def execute(g: GraphT, op : Operation) = (op, extremity) match {
    case (Add, _) => super.execute(g, op)
    case (Remove, _) => edge.create(g)
  }

}

case class TTTypeRedirection[Kind <: NodeKind[Kind], T2]
(typed : NodeId[Kind],
 typ : TypeHolder[Kind],
 oldUsee: NodeId[Kind],
 newUsee : NodeId[Kind])
  extends TransformationTarget[Kind, T2]{

  override def execute(g: AccessGraph[Kind, T2], op: Operation) = op match {
    case Add => g.changeType(typed, typ, oldUsee, newUsee)
    case Remove => g.changeType(typed, typ, newUsee, oldUsee)
  }
}

/*
case class TTDependency[Kind <: NodeKind[Kind], T](dominant : AGEdge[Kind],
                                                   dominated : AGEdge[Kind])
  extends TransformationTarget[Kind,T]{

  def execute(g: GraphT, op : Operation) = ???
}

case class TTAbstraction[Kind <: NodeKind[Kind], T](impl: NodeId[Kind],
                                                    abs: NodeId[Kind],
                                                    policy: AbstractionPolicy)
  extends TransformationTarget[Kind, T]{

  def execute(g: GraphT, op : Operation) = ???
}


case class TTConstraint[Kind <: NodeKind[Kind]](ct : Constraint[Kind],
                                                friend : AGNode[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = ???
}*/