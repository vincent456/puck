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

package puck.graph.constraints.search

import puck.graph._
import puck.graph.comparison.Mapping
import puck.graph.transformations.Transformation
import puck.search.{Evaluator, SearchState}

import scalaz.{-\/, \/-}

object DecoratedGraphEvalutaor {

  //Constructeurs d'Evaluator pour Dependency Graph

  //comparaison directe de deux graphes de dépendances sans noeud virtuel
  def equalityByMapping[T](fitness : DependencyGraph => Double) : Evaluator[DecoratedGraph[T]] =
    new DecoratedGraphEvalutaor(fitness,  Mapping.equals)

  //on compare deux graphes de dépendances issus du même graphe initial en comparant leurs plans de refactoring
  //moins coûteux que de les comparer directement - initialement prévu pour le backtrack
  def equalityBasedOnRecording
  ( initialRecord : Seq[Transformation],
    fitness : DependencyGraph => Double) : Evaluator[SResult] =
    new DecoratedGraphEvalutaor(fitness,  DependencyGraph.areEquivalent(initialRecord, _, _))

}

class DecoratedGraphEvalutaor[T]
( val fitness : DependencyGraph => Double,
  val equals : ( DependencyGraph,  DependencyGraph) => Boolean)
  extends Evaluator[DecoratedGraph[T]] {


  def evaluate(s : SearchState[DecoratedGraph[T]]): Double =
    s.loggedResult.value match {
      case -\/(err) => 0
      case \/-(res) =>
        val g = graphOfResult(res)
        fitness(g)
    }

  def equals(s1 : SearchState[DecoratedGraph[T]], s2 : SearchState[DecoratedGraph[T]] ): Boolean =
    (s1.loggedResult.value, s2.loggedResult.value) match {
      case (\/-(res1), \/-(res2)) => equals(graphOfResult(res1), graphOfResult(res2))
      case _ => false
    }

}


