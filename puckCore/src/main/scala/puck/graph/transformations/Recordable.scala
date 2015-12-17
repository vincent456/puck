package puck.graph.transformations

import puck.graph.DependencyGraph

abstract class Recordable extends Serializable {
  def redo(g: DependencyGraph) : DependencyGraph
}

sealed abstract class Direction {
  def reverse : Direction
  def productPrefix : String
}
case object Regular extends Direction {
  def reverse = Reverse
}
case object Reverse extends Direction{
  def reverse = Regular
}

case class Transformation
(direction : Direction,
 operation : Operation) extends Recordable {

  def redo(g: DependencyGraph) : DependencyGraph = operation.execute(g, direction)
  def undo(g: DependencyGraph) : DependencyGraph = operation.execute(g, direction.reverse)

}

case object MileStone extends Recordable {
  def redo(g: DependencyGraph) = g.mileStone
}
case class Comment(msg : String) extends Recordable {
  def redo(g: DependencyGraph) = g.comment(msg)
}