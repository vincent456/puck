package puck.graph.transformations

import puck.graph._
import puck.graph.constraints.AbstractionPolicy

/**
 * Created by lorilan on 27/10/14.
 */

object Recording {
  def apply() = new Recording(Seq())
}

class Recording
(private [this] val record : Seq[Transformation]) extends Iterable[Transformation] {

  type NIdT = NodeId
  type EdgeT = DGEdge
  type RecT = Recording
  def apply() = record


  def +:(r : Transformation) : Recording =
    new Recording(r +: record)

  override def iterator: Iterator[Transformation] = record.iterator
 /* def nonEmpty = record.nonEmpty
  def size = record.size*/

  def addNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
    Transformation(Add, TTNode(id, name, kind, styp, mutable)) +: this

  def removeNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
    Transformation(Remove, TTNode(id, name, kind, styp, mutable)) +: this

  def addEdge(edge : EdgeT) : RecT =
    Transformation(Add, TTEdge(edge)) +: this

  def removeEdge(edge : EdgeT) : RecT=
    Transformation(Remove, TTEdge(edge)) +: this

  def changeEdgeTarget(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge(edge, Target(newTarget))
              else TTRedirection(edge, Target(newTarget))
    Transformation(Add, red) +: this
  }

  def changeEdgeSource(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge(edge, Source(newTarget))
    else TTRedirection(edge, Source(newTarget))
    Transformation(Add, red) +: this
  }
  def addTypeChange( typed : NIdT,
                     typ: TypeHolder,
                     oldUsee: NIdT,
                     newUsee : NIdT) : RecT =
    Transformation(Add, TTTypeRedirection(typed, typ, oldUsee, newUsee)) +: this

  def addAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Add, TTAbstraction(impl, abs, absPolicy)) +: this

  def removeAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Remove, TTAbstraction(impl, abs, absPolicy)) +: this

}

sealed abstract class Operation {
  def reverse : Operation
  def productPrefix : String
}
case object Add extends Operation {
  def reverse = Remove
}
case object Remove extends Operation{
  def reverse = Add
}

case class Transformation
(operation : Operation,
 target : TransformationTarget){
  type GraphT = DependencyGraph

  def redo(g: GraphT) = target.execute(g, operation)
  def undo(g: GraphT) = target.execute(g, operation.reverse)

}



