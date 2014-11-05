package puck.graph.immutable.transformations

import puck.graph.immutable._
import puck.graph.immutable.AccessGraph.NodeId

/**
 * Created by lorilan on 27/10/14.
 */

object Recording {
  def apply[Kind <: NodeKind[Kind], T]() = new Recording[Kind, T](Seq())
}

class Recording[Kind <: NodeKind[Kind], T]
(private [this] val record : Seq[Transformation[Kind, T]]) {

  type NIdT = NodeId[Kind]
  type EdgeT = AGEdge[Kind]
  type RecT = Recording[Kind, T]
  def apply() = record


  def +:(r : Transformation[Kind, T]) : Recording[Kind, T] =
    new Recording(r +: record)

  def nonEmpty = record.nonEmpty
  def size = record.size

  def addNode(id : NIdT, name : String, kind : Kind, styp: TypeHolder[Kind],
              mutable : Boolean, t : T) : RecT =
    Transformation(Add, TTNode(id, name, kind, styp, mutable, t)) +: this

  def addEdge(edge : EdgeT) : RecT =
    Transformation(Add, TTEdge[Kind,T](edge)) +: this

  def removeEdge(edge : EdgeT) : RecT=
    Transformation(Remove, TTEdge[Kind,T](edge)) +: this

  def changeEdgeTarget(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge[Kind,T](edge, Target(newTarget))
              else TTRedirection[Kind, T](edge, Target(newTarget))
    Transformation(Add, red) +: this
  }

  def changeEdgeSource(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge[Kind,T](edge, Source(newTarget))
    else TTRedirection[Kind, T](edge, Source(newTarget))
    Transformation(Add, red) +: this
  }
  def addTypeChange( typed : NIdT,
                     typ: TypeHolder[Kind],
                     oldUsee: NIdT,
                     newUsee : NIdT) : RecT =
    Transformation(Add, TTTypeRedirection[Kind,T](typed, typ, oldUsee, newUsee)) +: this

/*  def addAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Add(), TTAbstraction[Kind,T](impl, abs, absPolicy)) +: this*/
}

sealed abstract class Operation {
  def reverse : Operation
}
case object Add extends Operation {
  def reverse = Remove
}
case object Remove extends Operation{
  def reverse = Add
}

case class Transformation[Kind <: NodeKind[Kind], T]
(operation : Operation,
 target : TransformationTarget[Kind, T]){
  type GraphT = AccessGraph[Kind,T]

  def undo(g: GraphT)= target.execute(g, operation)
  def redo(g: GraphT) = target.execute(g, operation.reverse)
}



