package puck.graph.immutable.transformations

import puck.graph.constraints.AbstractionPolicy
import puck.graph.immutable._

import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 27/10/14.
 */

object Recording {
  def apply[Kind <: NodeKind[Kind], T]() = new Recording[Kind, T](Seq())
}

class Recording[Kind <: NodeKind[Kind], T]
(private [this] val record : Seq[Recordable[Kind, T]]) {

  type NIdT = NodeId[Kind]
  type EdgeT = AGEdge[Kind]
  type RecT = Recording[Kind, T]
  def apply(){} // mutable plug method

  def +:(r : Recordable[Kind, T]) : Recording[Kind, T] =
    new Recording(r +: record)

  def nonEmpty = record.nonEmpty
  def size = record.size

  def produceSameGraph( other : Recording[Kind, T]) : Boolean = false

  def addNode(id : NIdT,
                                    name : String,
                                    kind : Kind,
                                    styp: TypeHolder[Kind],
                                    mutable : Boolean,
                                     t : T) : RecT =
    Transformation(Add(), TTNode(id, name, kind, styp, mutable, t)) +: this

  def addEdge(edge : EdgeT) : RecT =
    Transformation(Add(), TTEdge[Kind,T](edge)) +: this

  def removeEdge(edge : EdgeT) : RecT=
    Transformation(Remove(), TTEdge[Kind,T](edge)) +: this

  def changeEdgeTarget(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge[Kind,T](edge, Target(newTarget))
              else TTRedirection[Kind, T](edge, Target(newTarget))
    Transformation(Add(), red) +: this
  }

  def changeEdgeSource(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge[Kind,T](edge, Source(newTarget))
    else TTRedirection[Kind, T](edge, Source(newTarget))
    Transformation(Add(), red) +: this
  }
  def addTypeChange( typed : NIdT,
                     typ: TypeHolder[Kind],
                     oldUsee: NIdT,
                     newUsee : NIdT) : RecT =
    Transformation(Add(), TTTypeRedirection[Kind,T](typed, typ, oldUsee, newUsee)) +: this

  def addAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Add(), TTAbstraction[Kind,T](impl, abs, absPolicy)) +: this
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

sealed abstract class Recordable[Kind <: NodeKind[Kind], T]{
  type GraphT = AccessGraph[Kind,T]
  def undo(g: GraphT) : GraphT
  def redo(g: GraphT) : GraphT
}

case class Transformation[Kind <: NodeKind[Kind], T](operation : Operation,
                                            target : TransformationTarget[Kind, T])
  extends Recordable[Kind, T]{
  def undo(g: GraphT)= target.execute(g, operation)
  def redo(g: GraphT) = target.execute(g, operation.reverse)
}


/*case class UndoBreakPoint[Kind <: NodeKind](id : Int)
  extends Recordable[Kind] {
  def undo(){}
  def redo(){}
}*/

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
    case Add() => g.addNode(id, name, kind, styp, mutable, t)
    case Remove() => g.removeNode(id)
  }
}
case class TTEdge[Kind <: NodeKind[Kind], T](edge : AGEdge[Kind])
  extends TransformationTarget[Kind, T]{

  def execute(g: GraphT, op : Operation) = op match {
    case Add() =>edge.create[T](g)
    case Remove() => edge.delete(g)
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
    case (Add(), Target(newTarget)) => edge.changeTarget(g, newTarget)
    case (Remove(), Target(newTarget)) => AGEdge(edge.kind, edge.source, newTarget).changeTarget(g, edge.target)
    case (Add(), Source(newSource)) => edge.changeSource(g, newSource)
    case (Remove(),Source(newSource)) => AGEdge(edge.kind, newSource, edge.target).changeSource(g, edge.source)
  }
}

class RedirectionWithMerge[Kind <: NodeKind[Kind], T](edge : AGEdge[Kind],
                                                   extremity : Extremity[Kind])
  extends TTRedirection[Kind, T](edge, extremity){

  override def execute(g: GraphT, op : Operation) = (op, extremity) match {
    case (Add(), _) => super.execute(g, op)
    case (Remove(), _) => edge.create(g)
  }

}

case class TTTypeRedirection[Kind <: NodeKind[Kind], T2]
(typed : NodeId[Kind],
 typ : TypeHolder[Kind],
 oldUsee: NodeId[Kind],
 newUsee : NodeId[Kind])
  extends TransformationTarget[Kind, T2]{

  override def execute(g: AccessGraph[Kind, T2], op: Operation) = op match {
    case Add() => g.changeType(typed, typ, oldUsee, newUsee)
    case Remove() => g.changeType(typed, typ, newUsee, oldUsee)
  }
}

case class TTDependency[Kind <: NodeKind[Kind], T](dominant : AGEdge[Kind],
                                          dominated : AGEdge[Kind])
  extends TransformationTarget[Kind,T]{

  def execute(g: GraphT, op : Operation) = ???/*{
    val g = dominant.source.graph
    op match {
      case Add() => g.addUsesDependency(dominant, dominated)
      case Remove() => g.removeUsesDependency(dominant, dominated)
    }
  }*/
}

case class TTAbstraction[Kind <: NodeKind[Kind], T](impl: NodeId[Kind],
                                           abs: NodeId[Kind],
                                           policy: AbstractionPolicy)
  extends TransformationTarget[Kind, T]{

  def execute(g: GraphT, op : Operation) = ??? /*op match {
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
