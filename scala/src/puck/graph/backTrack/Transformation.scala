package puck.graph.backTrack

import puck.graph.{AGEdge, AGNode}
import puck.graph.constraints.{AbstractionPolicy, Constraint}

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

sealed abstract class TransformationTarget
case class TTNode(n : AGNode) extends TransformationTarget
case class TTEdge(e : AGEdge) extends TransformationTarget
case class TTDependency(dominant : AGEdge, dominated : AGEdge) extends TransformationTarget
case class TTAbstraction(impl: AGNode, abs: AGNode,
                       policy: AbstractionPolicy) extends TransformationTarget
case class TTConstraint(ct : Constraint) extends TransformationTarget


sealed abstract class Recordable{
  def undo() : Unit
  def redo() : Unit
  def copy() : Recordable = this

}

abstract class Transformation extends Recordable{
  def target : TransformationTarget
  def operation : Operation
  override def copy() : Transformation = this
}

abstract class Reverse[T <: Transformation] extends Transformation{
  val reverse : T

  def target = reverse.target
  def operation = reverse.operation.reverse
  def undo() = reverse.redo()
  def redo() = reverse.undo()
}

/*class CompositeTransformation extends Transformation{

  val sequence = new mutable.Stack[Transformation]()

  override def toString =
    sequence.mkString("CompositeTransformation[\n", ",\n", "]\n")

  def push(t : Transformation) = sequence.push(t)
  def pop() = sequence.pop()


  def undo() { sequence.iterator.foreach(_.undo()) }
  def redo() { sequence.reverseIterator.foreach(_.redo()) }

  override def copy() : CompositeTransformation = {
    val ct = new CompositeTransformation()
    sequence.reverseIterator.foreach(ct.push)
    ct
  }
}*/

abstract class BreakPoint extends Recordable {
  def undo(){}
  def redo(){}
}
case class UndoBreakPoint() extends BreakPoint
case class SearchStateBreakPoint() extends BreakPoint

case class AddNode( node : AGNode) extends Transformation {
  override def toString = "add node %s".format(node)

  val target = TTNode(node)
  val operation = Add()

  def undo(){ node.graph.remove(node) }
  def redo(){ node.graph.addNode(node) }
}

case class RemoveNode(node : AGNode) extends Reverse[AddNode]{
  override def toString = "remove node %s".format(node)
  val reverse = new AddNode( node)
}



case class AddEdge( edge : AGEdge) extends Transformation {

  val target = TTEdge(edge)
  val operation = Add()

  override def toString = "add edge " + edge
  def undo(){ edge.delete() }
  def redo(){ edge.create() }
}


case class RemoveEdge( edge : AGEdge) extends Reverse[AddEdge]{
  override def toString = "remove edge " + edge
  val reverse = new AddEdge(edge)
}

case class AddEdgeDependency(dominant : AGEdge, dominated : AGEdge) extends Transformation{
  //override def toString = "add edge dependency ( %s , %s) ".format(dominant, dominated)

  val target = TTDependency(dominant, dominated)
  val operation = Add()

  def undo(){
    val g = dominant.source.graph
    g.removeUsesDependency(dominant, dominated)
  }
  def redo(){
    val g = dominant.source.graph
    g.addUsesDependency(dominant, dominated)
  }
}
case class RemoveEdgeDependency(dominant : AGEdge, dominated : AGEdge)
  extends Reverse[AddEdgeDependency]{
  //override def toString = "remove edge dependency ( %s , %s) ".format(dominant, dominated)
  val reverse = new AddEdgeDependency(dominant, dominated)
}

case class RegisterAbstraction(impl : AGNode, abs :AGNode,
                           policy : AbstractionPolicy) extends Transformation {

  val target = TTAbstraction(impl, abs, policy)
  val operation = Add()

  def undo(){ impl.abstractions_-=(abs, policy) }
  def redo(){ impl.abstractions_+=(abs, policy) }
}
case class UnregisterAbstraction( impl : AGNode, abs : AGNode,
                             policy : AbstractionPolicy)
  extends Reverse[RegisterAbstraction]{
  val reverse = new RegisterAbstraction(impl, abs, policy)
}

case class AddFriend (ct : Constraint, friend : AGNode) extends Transformation{
  val target = TTConstraint(ct)
  val operation = Add()

  def undo(){ ct.friends -= friend }
  def redo(){ ct.friends += friend }
}

