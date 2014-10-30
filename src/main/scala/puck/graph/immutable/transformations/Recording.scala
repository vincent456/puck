package puck.graph.immutable.transformations

import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable._

import puck.graph.immutable.AccessGraph.NodeId



/**
 * Created by lorilan on 27/10/14.
 */

object Recording {
  def apply[Kind <: NodeKind[Kind]]() = new Recording[Kind](Seq())
}

class Recording[Kind <: NodeKind[Kind]]
(private [this] val record : Seq[Recordable[Kind]]) {

  def addNode(id : NodeId[Kind], name : String, kind : Kind) : Recording[Kind] =
    Transformation(Add(), TTNode(id, name, kind)) +: this

  def +:(r : Recordable[Kind]) : Recording[Kind] =
    new Recording(r +: record)
}

sealed abstract class Operation {
  def reverse : Operation
}
case class Add() extends Operation {
  def reverse = Remove()
}
case class Remove() extends Operation{
  def reverse = Add()
}

sealed abstract class Recordable[Kind <: NodeKind[Kind]]{
  def undo(g: AccessGraph[Kind]) : AccessGraph[Kind]
  def redo(g: AccessGraph[Kind]) : AccessGraph[Kind]
  def copy() : Recordable[Kind] = this
}

case class Transformation[Kind <: NodeKind[Kind]](operation : Operation,
                                            target : TransformationTarget[Kind])
  extends Recordable[Kind]{
  def redo(g: AccessGraph[Kind]) = target.execute(g, operation)
  def undo(g: AccessGraph[Kind]) = target.execute(g, operation.reverse)
  override def copy() : Transformation[Kind] = this
}


/*case class UndoBreakPoint[Kind <: NodeKind](id : Int)
  extends Recordable[Kind] {
  def undo(){}
  def redo(){}
}*/

sealed abstract class TransformationTarget[Kind <: NodeKind[Kind]]{
  def execute(g: AccessGraph[Kind], op : Operation) : AccessGraph[Kind]
}
case class TTNode[Kind <: NodeKind[Kind]]
( id : NodeId[Kind],
  name : String,
  kind : Kind)
  extends TransformationTarget[Kind]{

  def execute(g: AccessGraph[Kind], op : Operation) = op match {
    case Add() => g.addNode(id, name, kind)
    case Remove() => g.removeNode(id)
  }
}
case class TTEdge[Kind <: NodeKind[Kind]](edge : AGEdge[Kind])
  extends TransformationTarget[Kind]{

  def execute(g: AccessGraph[Kind], op : Operation) = ???/*op match {
    case Add() => edge.create()
    case Remove() => edge.delete()
  }*/
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

  def execute(g: AccessGraph[Kind], op : Operation) = ??? /*(op, extremity) match {
    case (Add(), Target(newTarget)) => edge.changeTarget(newTarget)
    case (Remove(), Target(newTarget)) => AGEdge(edge.kind, edge.source, newTarget).changeTarget(edge.target)
    case (Add(), Source(newSource)) => edge.changeSource(newSource)
    case (Remove(),Source(newSource)) => AGEdge(edge.kind, newSource, edge.target).changeSource(edge.source)
  }*/
}

class RedirectionWithMerge[Kind <: NodeKind[Kind]](edge : AGEdge[Kind],
                                                   extremity : Extremity[Kind])
  extends TTRedirection[Kind](edge, extremity){

  override def execute(g: AccessGraph[Kind], op : Operation) = ???/*(op, extremity) match {
    case (Add(), _) => super.execute(op)
    case (Remove(), _) => edge.create()
  }*/

}

/*case class TTTypeRedirection[Kind <: NodeKind, T <: Type[Kind, T]](kind : HasType[Kind,T],
                                                                         oldUsee : AGNode[Kind],
                                                                         newUsee : AGNode[Kind])
  extends TransformationTarget[Kind]{
  override def execute(op: Operation) = op match {
    case Add() => kind.redirectUses(oldUsee, newUsee)
    case Remove() => kind.redirectUses(newUsee, oldUsee)
  }
}*/

case class TTDependency[Kind <: NodeKind[Kind]](dominant : AGEdge[Kind],
                                          dominated : AGEdge[Kind])
  extends TransformationTarget[Kind]{

  def execute(g: AccessGraph[Kind], op : Operation) = ???/*{
    val g = dominant.source.graph
    op match {
      case Add() => g.addUsesDependency(dominant, dominated)
      case Remove() => g.removeUsesDependency(dominant, dominated)
    }
  }*/
}

case class TTAbstraction[Kind <: NodeKind[Kind]](impl: AGNode[Kind],
                                           abs: AGNode[Kind],
                                           policy: AbstractionPolicy)
  extends TransformationTarget[Kind]{

  def execute(g: AccessGraph[Kind], op : Operation) = ??? /*op match {
    case Add() => impl.abstractions_+=(abs, policy)
    case Remove() => impl.abstractions_-=(abs, policy)
  }*/
}

/*
case class TTConstraint[Kind <: NodeKind[Kind]](ct : Constraint[Kind],
                                                friend : AGNode[Kind])
  extends TransformationTarget[Kind]{

  def execute(op : Operation) = op match {
    case Add() => ct.friends += friend
    case Remove() => ct.friends -= friend
  }
}*/
