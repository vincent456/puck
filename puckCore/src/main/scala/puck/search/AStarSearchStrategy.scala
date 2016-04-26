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

import puck.graph.LoggedTry

import scala.collection.mutable

/**
  * Created by Loïc Girault on 16/11/15.
  */
class AStarSearchStrategy[T]
( evaluator: Evaluator [T],
  depthCost : Int = 1
  ) extends SearchStrategy[T] {


  implicit object SearchStateOrdering extends Ordering[SearchState[T]]{

    def evaluateWithDepthPenaly(x: SearchState[T]) =
      // Math.max(evaluator.evaluateInt(x) + x.depth * depthCost, 0)
      x.depth

    override def compare(x: SearchState[T], y: SearchState[T]): Int =
      evaluateWithDepthPenaly(y) - evaluateWithDepthPenaly(x)
  }

  val remainingStates = new mutable.PriorityQueue[SearchState[T]]()

  def addState(s: SearchState[T]): Unit =
    ignore(remainingStates += s)

  def currentState: SearchState[T] =
    remainingStates.head

  def addState(currentResult: LoggedTry[T], choices: Seq[LoggedTry[T]]): Unit =
    ignore(remainingStates += currentState.createNextState(currentResult, choices))

  def nextState: SearchState[T] = {
    if (remainingStates.head.triedAll) remainingStates.dequeue()
    remainingStates.head
  }

  def canContinue: Boolean =
    !remainingStates.head.triedAll || remainingStates.tail.nonEmpty
}
