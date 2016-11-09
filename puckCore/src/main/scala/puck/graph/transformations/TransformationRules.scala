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
package graph
package transformations

import puck.graph.Type.OnImplemNotFound
import puck.graph.transformations.rules._
import puck.util.LoggedEither._
import scalaz.std.list._

class TransformationRules
( mergingCandidatesFinder : MergingCandidatesFinder,
  val rename : Renamer,
  val abstracter : Abstract,
  val intro : Intro) {



  def mergeMatcherInstances = mergingCandidatesFinder.mergeMatcherInstances

  lazy val merge = new Merge(mergingCandidatesFinder)
  val remove = Remove
  val redirection = Redirection
  val move = Move

  def makeSuperType( g: DependencyGraph, sub : NodeId, sup : NodeId,
                     failOnImplemNotFound : Boolean = false): LoggedTG =
  if(!failOnImplemNotFound) LoggedSuccess(g.addIsa(NamedType(sub), NamedType(sup)))
  else {
    val subNode = g.getConcreteNode(sub)
    val supNode = g.getConcreteNode(sup)
    val subMethods = g.instanceValuesWithType(sub)
    val supMethods = g.instanceValuesWithType(sup)

    Type.everyMethodIsOverridedIn(g, supMethods, subMethods) map {
      _ => g.addIsa(NamedType(sub), NamedType(sup))
    }
  }
}
