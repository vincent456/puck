package puck.graph

import puck.graph.constraints.AbstractionPolicy

import scala.collection.mutable

/**
 * Created by lorilan on 11/06/14.
 */

sealed abstract class Transformation {
  def undo() : Unit
}

class CompositeTransformation extends Transformation{

  val sequence = new mutable.Stack[Transformation]()

  def push(t : Transformation) = sequence.push(t)

  def undo(){
    while(sequence.nonEmpty){
      sequence.pop().undo()
    }
  }
}

class AddNode( node : AGNode) extends Transformation {
  def undo(){
    val g = node.graph
    g.nodes -= node
  }
}
class RemoveNode( node : AGNode) extends Transformation{
  def undo(){
    val g = node.graph
    g.nodes += node
  }
}
class AddEdge( edge : AGEdge) extends Transformation {
  def undo(){ edge.delete()}
}
class RemoveEdge( edge : AGEdge) extends Transformation{
  def undo(){ edge.create()}
}
class AddEdgeDependancy( dominant : AGEdge, dominated : AGEdge) extends Transformation{
  def undo(){
    val g = dominant.source.graph
    g.removeUsesDependency(dominant.source, dominant.target,
      dominated.source, dominated.target)
  }
}
class RemoveEdgeDependancy( dominant : AGEdge, dominated : AGEdge) extends Transformation{
  def undo(){
    val g = dominant.source.graph
    g.addUsesDependency(dominant.source, dominant.target,
      dominated.source, dominated.target)
  }
}

class RegisterAbstraction( impl : AGNode, abs :AGNode,
                           policy : AbstractionPolicy) extends Transformation {
  def undo(){
    impl.abstractions_-=(abs, policy)
  }
}
class UnregisterAbstraction( impl : AGNode, abs : AGNode,
                             policy : AbstractionPolicy) extends Transformation{
  def undo(){
    impl.abstractions_+=(abs, policy)
  }
}


