package puck.graph.mutable.backTrack

import puck.graph.mutable._
import puck.graph.mutable.constraints.{Constraint, AbstractionPolicy}

/**
 * Created by lorilan on 08/10/14.
 */

sealed abstract class TransformationTarget[Kind <: NodeKind[Kind]]{
  def execute(op : Operation) : Unit
}
case class TTNode[Kind <: NodeKind[Kind]](node : AGNode[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = op match {
    case Add() => node.graph.addNode(node)
    case Remove() => node.graph.remove(node)
  }
}
case class TTEdge[Kind <: NodeKind[Kind]](edge : AGEdge[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = op match {
    case Add() => edge.create()
    case Remove() => edge.delete()
  }
}

sealed abstract class Extremity[Kind <: NodeKind[Kind]]{
  val node : AGNode[Kind]
  def create(n : AGNode[Kind]) : Extremity[Kind]
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]): AGNode[K]*/
}
case class Source[Kind <: NodeKind[Kind]](node : AGNode[Kind]) extends Extremity[Kind]{
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]) = e.source*/
  def create(n : AGNode[Kind]) : Extremity[Kind] = Source(n)
}
case class Target[Kind <: NodeKind[Kind]](node : AGNode[Kind]) extends Extremity[Kind]  {
  /*def apply[K <: NodeKind[K]](e : AGEdge[K]) = e.target*/
  def create(n : AGNode[Kind]) : Extremity[Kind] = Target(n)
}

case class TTRedirection[Kind <: NodeKind[Kind]](edge : AGEdge[Kind],
                                                 extremity : Extremity[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = (op, extremity) match {
    case (Add(), Target(newTarget)) => edge.changeTarget(newTarget)
    case (Remove(), Target(newTarget)) => AGEdge(edge.kind, edge.source, newTarget).changeTarget(edge.target)
    case (Add(), Source(newSource)) => edge.changeSource(newSource)
    case (Remove(),Source(newSource)) => AGEdge(edge.kind, newSource, edge.target).changeSource(edge.source)
  }
}

class RedirectionWithMerge[Kind <: NodeKind[Kind]](edge : AGEdge[Kind],
                                                   extremity : Extremity[Kind])
  extends TTRedirection[Kind](edge, extremity){

  override def execute(op : Operation) = (op, extremity) match {
    case (Add(), _) => super.execute(op)
    case (Remove(), _) => edge.create()
  }

}

case class TTTypeRedirection[Kind <: NodeKind[Kind], T <: Type[Kind, T]](kind : HasType[Kind,T],
                                                                         oldUsee : AGNode[Kind],
                                                                         newUsee : AGNode[Kind])
  extends TransformationTarget[Kind]{
  override def execute(op: Operation) = op match {
    case Add() => kind.redirectUses(oldUsee, newUsee)
    case Remove() => kind.redirectUses(newUsee, oldUsee)
  }
}

case class TTDependency[Kind <: NodeKind[Kind]](dominant : AGEdge[Kind],
                                                dominated : AGEdge[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = {
    val g = dominant.source.graph
    op match {
      case Add() => g.addUsesDependency(dominant, dominated)
      case Remove() => g.removeUsesDependency(dominant, dominated)
    }
  }
}

case class TTAbstraction[Kind <: NodeKind[Kind]](impl: AGNode[Kind],
                                                 abs: AGNode[Kind],
                                                 policy: AbstractionPolicy)
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = op match {
    case Add() => impl.abstractions_+=(abs, policy)
    case Remove() => impl.abstractions_-=(abs, policy)
  }
}

case class TTConstraint[Kind <: NodeKind[Kind]](ct : Constraint[Kind],
                                                friend : AGNode[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = op match {
    case Add() => ct.friends += friend
    case Remove() => ct.friends -= friend
  }
}