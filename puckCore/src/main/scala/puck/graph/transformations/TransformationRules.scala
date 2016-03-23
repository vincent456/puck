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
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.rules._
import puck.util.LoggedEither._
import scalaz.std.list._
import ShowDG._

class TransformationRules
( mergingCandidatesFinder : MergingCandidatesFinder,
  val rename : Renamer,
  val abstracter : Abstract,
  val intro : Intro) {



  //def findMergingCandidate = mergingCandidatesFinder.find _
  def mergeMatcherInstances = mergingCandidatesFinder.mergeMatcherInstances

  lazy val merge = new Merge(mergingCandidatesFinder)
  def removeConcreteNode = merge.removeConcreteNode _

  val redirection = Redirection
  val move = Move

//  def addHideFromRootException(g : DependencyGraph, node : NodeId, friend : NodeId): DependencyGraph =
//    g.newGraph(constraints = g.constraints.addHideFromRootException(g, node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(g, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(g, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/

  def makeSuperType(g: DependencyGraph, sub : NodeId, sup : NodeId)
                   ( onImplemNotFound : OnImplemNotFound = Type.ignoreOnImplemNotFound): LoggedTG = {
    val subNode = g.getConcreteNode(sub)
    val supNode = g.getConcreteNode(sup)
    if(!g.canBe(subNode, supNode))
      LoggedError(s"${(g, sub).shows} cannot be ${(g, sup).shows}")
    else {

      val subMethods = g.content(sub).toList map g.typedNode
      val supMethods = g.content(sup).toList map g.typedNode

      Type.findAndRegisterOverridedInList(g, supMethods, subMethods)(
        onImplemNotFound) map ( _.addIsa(sub, sup).
                addAbstraction(sub, AccessAbstraction(sup, SupertypeAbstraction))
        ) flatMap {
        g =>
          val overloadedMethod =
            subMethods filter {
              case ((m, _)) => g.abstractions(m.id) exists {
                case AccessAbstraction(supMethId, SupertypeAbstraction)
                  if g.contains(sup, supMethId) => true
                case _ => false

              }
            }
          overloadedMethod.foldLoggedEither(g){
            case (g0, (m, _)) =>
              abstracter.redirectTypeUseInParameters(g0, m, subNode, supNode)
          }
      }

    }
  }

}
