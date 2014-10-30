package puck.graph.mutable.backTrack

import puck.graph._
import puck.graph.mutable.NodeKind
import puck.graph.mutable.constraints.{AbstractionPolicy, Constraint}

/**
 * Created by lorilan on 11/06/14.
 */

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
  def undo() : Unit
  def redo() : Unit
  def copy() : Recordable[Kind] = this

}

case class Transformation[Kind <: NodeKind[Kind]](operation : Operation,
                                                  target : TransformationTarget[Kind])
  extends Recordable[Kind]{
  def redo() = target.execute(operation)
  def undo() = target.execute(operation.reverse)
  override def copy() : Transformation[Kind] = this
}


case class UndoBreakPoint[Kind <: NodeKind[Kind]](id : Int)
  extends Recordable[Kind] {
  def undo(){}
  def redo(){}
}
