package puck.search

import puck.searchNew.SearchState

/**
 * Created by lorilan on 30/10/15.
 */
abstract class Evaluator[T]{

  type StateT = SearchState[T]

  def filterDifferentStates (l : Seq[StateT]) : Seq[StateT] = {
    def aux(l : Seq[StateT], acc : Seq[StateT]) : Seq[StateT] = {
      if (l.nonEmpty) {
        aux(l.tail,
          if (!l.tail.exists { st => equals(st, l.head)})
            l.head +: acc
          else acc)
      }
      else acc
    }
    aux(l, Seq())
  }

  def evaluatedMap(l : Seq[StateT], precision : Int = 2) : Map[Int, Seq[StateT]] = {
      this.precision = Math.pow(10, precision.toDouble).toInt
      l.groupBy(evaluateInt)
  }

  def evaluate(r : StateT) : Double
  def equals(r1 : StateT, r2 : StateT) : Boolean

  private var precision = 100
  def evaluateInt(r : StateT) : Int = (evaluate(r) * precision).toInt

  def evaluatedDifferentStatesMap(l : Seq[StateT]) :  Map[Int, Seq[StateT]] =
    evaluatedMap(l) mapValues filterDifferentStates

  def evaluatedSeq(l : Seq[StateT], precision : Int = 2) : Seq[(Int, StateT)] ={
    this.precision = Math.pow(10, precision.toDouble).toInt
    l map (s => (evaluateInt(s), s)) sortBy(_._1)
  }

  def bests(l : Seq[StateT], numBestsMax : Int) : Seq[StateT] =
    evaluatedSeq(l) sortBy (_._1) map (_._2) take numBestsMax
}