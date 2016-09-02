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

package puck.graph.transformations.rules

import puck.graph.transformations.TransformationRules
import puck.graph._
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.std.set._

/**
  * Created by Loïc Girault on 06/07/16.
  */
object ReplaceInheritanceByDelegation {

  def supToDelegate(g : DependencyGraph, sub : NodeId, sup : NodeId) : LoggedTG =
    LoggedError("not implemented")

  //(present, absent and used outside of the class)
  def sortMethods
  ( g : DependencyGraph,
    subCandidate : NodeId,
    sup : NodeId ) : (Seq[NodeId], Seq[NodeId]) = {
    g.content(subCandidate).foldLeft((List[NodeId](), List[NodeId]())) {
      case (acc @ (present, absent), m) =>
        g.abstractions(m) find {
          case AccessAbstraction(mabs, SupertypeAbstraction) => g.contains(sup, mabs)
          case _ => false
        } match {
          case None =>
            val usedOutsideOfClass = g.usersOf(m).exists(user => !g.contains_*(subCandidate, user))
            if( usedOutsideOfClass) (present, m +: absent)
            else acc
          case Some(AccessAbstraction(mabs, _)) => (mabs +: present, absent)
          case _ => puck.error()
        }
    }
  }
  def subsToDelegate(g : DependencyGraph,
                     subs : Seq[NodeId],
                     sup : NodeId,
                     delegateContainer : NodeId,
                     delegateKind : NodeKind)
                    (rules : TransformationRules): LoggedTG = {
    //
    val (present, absent) = subs.foldLeft(Set[NodeId](), List[NodeId]()) {
      case ((presentAcc, absentAcc), sub) =>
        val (present0, absent0) = sortMethods(g, sub, sup)
        (presentAcc ++ present0, absentAcc ++ absent0)
    }

    println("present = " + present)
    val (presentAbstract, presentConcrete) = present.partition(g.definitionOf(_).isEmpty)

    if(presentConcrete.nonEmpty || absent.nonEmpty) LoggedError("presentConcrete.nonEmpty || absent.nonEmpty")
    else {
      val supNode = g.getConcreteNode(sup)
        val (cn , g1) =  g.addConcreteNode(supNode.name + "SubDelegate", delegateKind)
        println("presentAbstract = " + presentAbstract)
        val (implAbs, g2) =
          presentAbstract.foldLeft((List[NodeIdP](), g1.addEdge(Contains(delegateContainer, cn.id)))){
            case ((implAbsAcc, g0), nid) =>
              val n = g0.getConcreteNode(nid)
              val (AccessAbstraction(mabs, _), g1) = rules.abstracter.createAbsNodeAndUse(g0, n, n.kind, DelegationAbstraction)
              ( (n.id, mabs) +: implAbsAcc,
              g1.addEdge(Contains(sup, mabs)).
                changeSource(Contains(sup, nid), cn.id))
          }

      val g3 = subs.foldLeft(g2){
        case (g0, sub) =>
          g0.removeEdge(Isa(sub, sup)).addEdge(Isa(sub, cn.id))
      }
      implAbs.foldLoggedEither(g3){
        case (g0, (impl, abs)) =>
          val absDef = g0 definitionOf_! abs
           g0.usersOf(impl).foldLoggedEither(g0){
             case (g00, user) =>
               if(user != absDef &&
                 g00.uses(user, impl)) rules.redirection.redirectInstanceUsesTowardAbstractionInAnotherTypeAndPropagate(g00, (user, impl), sup)
               else LoggedSuccess(g00)
           }

      }

    }
  }

}
