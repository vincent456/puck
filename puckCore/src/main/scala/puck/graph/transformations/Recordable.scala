/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph.transformations

import puck.graph._

abstract class Recordable extends Serializable {
  def redo(g: DependencyGraph) : DependencyGraph
  def reverse : Recordable = this
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

  override def reverse : Transformation = Transformation(direction.reverse, operation)

}

case object MileStone extends Recordable {
  def redo(g: DependencyGraph) = g.mileStone
}
case class Comment(msg : String) extends Recordable {
  def redo(g: DependencyGraph) = g.comment(msg)
}