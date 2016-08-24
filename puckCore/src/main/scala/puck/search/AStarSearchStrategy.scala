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

package puck
package search

import scala.collection.mutable

/**
  * Created by Loïc Girault on 16/11/15.
  */

object AStarSearchStrategy {
  def ordering[T](evaluator: Evaluator [T])= new Ordering[SearchState[T]]{

    def evaluateWithDepthPenalty(x: SearchState[T]) : Int =
      Math.max(evaluator.evaluateInt(x) + x.depth, 0)

    override def compare(sx: SearchState[T], sy: SearchState[T]): Int =
      evaluateWithDepthPenalty(sx) compareTo evaluateWithDepthPenalty(sy)
  }
}
class AStarSearchStrategy[T]
( evaluator: Evaluator [T],
  maxDepth : Int = 100, // ajouté par Mikal
  maxSize : Int = 1000
  ) extends SearchStrategy[T] {

  implicit val SearchStateOrdering = AStarSearchStrategy.ordering(evaluator)

  // var remainingStates = new mutable.PriorityQueue[SearchState[T]]()(SearchStateOrdering.reverse)
  var remainingStates : mutable.SortedSet[SearchState[T]] = new mutable.TreeSet[SearchState[T]]()(SearchStateOrdering)

  def isSuccess(s: SearchState[T]) =
    s.loggedResult.value.isRight

  def addState(s: SearchState[T]): Unit =
    if (isSuccess(s) && (s.depth < maxDepth)) {
      remainingStates += s
      if (remainingStates.size > maxSize)
        puck.ignore(remainingStates.remove(remainingStates.last))
    }

  def popState() : SearchState[T] = {
    val res = remainingStates.head
    remainingStates.remove(remainingStates.head)
    return res
  }

  def canContinue: Boolean = remainingStates.nonEmpty
}
