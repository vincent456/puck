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

package puck.graph.constraints

import puck.graph._
import puck.graph.constraints.DecisionMaker.ChooseNodeKArg
import puck.graph.transformations.rules.CreateVarStrategy
import puck.util.Logged


// La définition d'un classe pour la fonction de prédicat
// permet de tagger de façon transparente la fonction avec un message de description
//trait NodePredicate {
//  def apply(dg : DependencyGraph, cn : ConcreteNode) : Boolean
//  override def toString : String = "NodePredicate"
//}

object DecisionMaker {
  type ChooseNodeKArg = Logged[Option[(DependencyGraph,NodeId)]]
}
trait DecisionMaker{

  def violationTarget
  ( lg : LoggedG)
  ( k: Logged[Option[ConcreteNode]] => Unit) : Unit

  def abstractionKindAndPolicy
  ( lg : LoggedG, impl : ConcreteNode)
  ( k : Logged[Option[(NodeKind, AbstractionPolicy)]] => Unit) : Unit

  def chooseNode
  ( lg : LoggedG, predicate : NodePredicate)
  ( k : ChooseNodeKArg => Unit) : Unit

  def chooseContainerKind
  ( lg : LoggedG, toBeContained : DGNode)
  ( k : Logged[Option[NodeKind]] => Unit) : Unit

  def selectExistingAbstraction
  ( lg : LoggedG, choices : Set[Abstraction])
  ( k : Logged[Option[Abstraction]] => Unit) : Unit

  def createVarStrategy
  ( g : LoggedG)
  ( k : Logged[CreateVarStrategy] => Unit) : Unit

}
