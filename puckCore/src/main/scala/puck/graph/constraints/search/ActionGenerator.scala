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

import puck.graph.{ConcreteNode, _}
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.{MutabilitySet, TransformationRules}

/**
  * Created by Loïc Girault on 10/05/16.
  */
trait ActionGenerator {
  val rules: TransformationRules
  val initialGraph: DependencyGraph
  val constraints: ConstraintsMaps
  val mutability : MutabilitySet.T

  def mapSeqLoggedTry[S, T](s : Seq[LoggedTry[S]], f : S => T) : Seq[LoggedTry[T]] =
    s map (_ map f)

  def decorate[T](s: Seq[LoggedTry[DependencyGraph]], t : T): Seq[LoggedTry[DecoratedGraph[T]]] =
    mapSeqLoggedTry[DependencyGraph, DecoratedGraph[T]](s, (_, t))

  val actionsGenerator = new SolvingActions(rules, constraints, mutability)

  def toSeqLTG[T](s: Seq[LoggedTry[(T, DependencyGraph)]]): Seq[LoggedTry[DependencyGraph]] =
    mapSeqLoggedTry[(T, DependencyGraph), DependencyGraph](s, _._2)

  val epsilon: DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    g => Seq(LoggedSuccess("Epsilon transition\n", g))


  def hostIntroAction(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.hostIntro(node) andThen toSeqLTG

  def moveAction(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.move(node) andThen toSeqLTG

  def moveContainerAction(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] = {
    dg =>
      val s = dg.container(node.id) map (id => Seq(dg.getConcreteNode(id))) getOrElse Seq()
      toSeqLTG(s flatMap (n => actionsGenerator.move(n)(dg)))
  }

  def redirectTowardAbstractions(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.redirectTowardExistingAbstractions(node)

  def absIntro(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] =
    actionsGenerator.absIntro(node) andThen toSeqLTG

  def hostAbsIntro(node : ConcreteNode) : DependencyGraph => Seq[LoggedTry[DependencyGraph]] ={
    dg =>
      val s = dg.container(node.id) map (id => Seq(dg.getConcreteNode(id))) getOrElse Seq()
      toSeqLTG(s flatMap (n => actionsGenerator.absIntro(n)(dg)))
  }

}
