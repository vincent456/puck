package puck.graph.transformations

import puck.graph._

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

object Transformation {

  val isAddRmOperation : Operation => Boolean = {
    case _ : AddRmOperation => true
    case _ => false
  }

  object Add {
    def unapply(t : Transformation) : Option[AddRmOperation] =
      t match {
        case Transformation(Regular, o : AddRmOperation) =>
          Some(o)
        case _ => None
      }
  }
  object Remove {
    def unapply(t : Transformation) : Option[AddRmOperation] =
      t match {
        case Transformation(Reverse, o : AddRmOperation) =>
          Some(o)
        case _ => None
      }
  }

  object Move {
    def unapply(t : Transformation) : Option[(NodeIdP, NodeId)] =
      t.operation match {
        case RedirectionOp(e @ Contains(oldSrc, tgt), Source(newSrc)) =>
          Some(((oldSrc, tgt), newSrc))
        //case RedirectionOp(e @ ContainsParam(oldSrc, tgt), Source(newSrc)) =>
        case _ => None
      }
  }

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