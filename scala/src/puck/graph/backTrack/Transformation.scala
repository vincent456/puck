package puck.graph.backTrack

import puck.graph.{AGEdge, AGNode}
import puck.graph.constraints.{AbstractionPolicy, Constraint}

import scala.collection.mutable

/**
 * Created by lorilan on 11/06/14.
 */

trait Transformation {
  def undo() : Unit
  def redo() : Unit
}

trait Reverse[T <: Transformation] extends Transformation{
  val reverse : T
  def undo() = reverse.redo()
  def redo() = reverse.undo()
}

class CompositeTransformation extends Transformation{

  val sequence = new mutable.Stack[Transformation]()

  def push(t : Transformation) = sequence.push(t)

  def undo() { sequence.iterator.foreach(_.undo()) }
  def redo() { sequence.reverseIterator.foreach(_.redo()) }
  /*def undo(){

    while(sequence.nonEmpty){
      val t = sequence.pop()
      t.undo()
    }

  }*/

}

class AddNode( node : AGNode) extends Transformation {
  override def toString = "add node %s".format(node)

  def undo(){ node.graph.remove(node) }
  def redo(){ node.graph.addNode(node) }
}

class RemoveNode(node : AGNode) extends Reverse[AddNode]{
  override def toString = "remove node %s".format(node)
  val reverse = new AddNode( node)
}

class AddEdge( edge : AGEdge) extends Transformation {
  override def toString = "add edge " + edge
  def undo(){ edge.delete() }
  def redo(){ edge.create() }
}

class RemoveEdge( edge : AGEdge) extends Reverse[AddEdge]{
  override def toString = "remove edge " + edge
  val reverse = new AddEdge(edge)
}

class AddEdgeDependency(dominant : AGEdge, dominated : AGEdge) extends Transformation{
  def undo(){
    val g = dominant.source.graph
    g.removeUsesDependency(dominant, dominated)
  }
  def redo(){
    val g = dominant.source.graph
    g.addUsesDependency(dominant, dominated)
  }
}
class RemoveEdgeDependency(dominant : AGEdge, dominated : AGEdge)
  extends Reverse[AddEdgeDependency]{
  val reverse = new AddEdgeDependency(dominant, dominated)
}

class RegisterAbstraction( impl : AGNode, abs :AGNode,
                           policy : AbstractionPolicy) extends Transformation {
  def undo(){ impl.abstractions_-=(abs, policy) }
  def redo(){ impl.abstractions_+=(abs, policy) }
}
class UnregisterAbstraction( impl : AGNode, abs : AGNode,
                             policy : AbstractionPolicy)
  extends Reverse[RegisterAbstraction]{
  val reverse = new RegisterAbstraction(impl, abs, policy)
}

class AddFriend (ct : Constraint, friend : AGNode) extends Transformation{
  def undo(){ ct.friends -= friend }
  def redo(){ ct.friends += friend }
}

