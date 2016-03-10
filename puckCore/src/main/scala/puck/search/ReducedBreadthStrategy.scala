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

/*
package puck.search


import puck.util.Logged
import scala.collection.mutable



class ReducedBreadthStrategy[T]
( val evaluator : Evaluator[T],
  val reductionPeriod : Int = 2,
  val numNewRoot : Int = 10
  ) extends SearchStrategy[T]{

  val remainingStates = mutable.Stack[SearchState[T]]()

  def canContinue : Boolean = remainingStates.nonEmpty || frozenStates.nonEmpty

  val frozenStates = mutable.Stack[SearchState[T]]()

  def currentState : SearchState[T] = remainingStates.head
  def addState(s : SearchState[T]) : Unit = remainingStates.push(s)

  var currentCheckPoint : Int = reductionPeriod



  override def createState[S <: StateCreator[T, S]](cr : Logged[T], choices : S): Unit ={
    val newState = remainingStates.head.createNextState[S](cr, choices)

    if ( newState.markedPointDepth >= currentCheckPoint)
      frozenStates push newState
    else remainingStates push newState

  }

  var i = 0
  override def oneStep(se : SearchEngine[T]) : Unit = {

    if (remainingStates.nonEmpty) {

      if (remainingStates.head.triedAll) remainingStates.pop()
      else remainingStates.head.executeNextChoice(se)

    }
    else if(frozenStates.nonEmpty) {

      i += 1
      remainingStates pushAll evaluator.bests(frozenStates.toSeq, numNewRoot)
      frozenStates.clear()
      currentCheckPoint += reductionPeriod
      this.oneStep(se)

    }
  }

}

*/
