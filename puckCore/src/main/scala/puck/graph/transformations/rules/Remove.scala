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

import puck.graph.{Contains, ContainsParam, InstanceValueDecl, NameSpace, Parameter, StaticValueDecl, TypeConstructor, TypeDecl, UnknownKindType, Uses, ValueDef, _}
import puck.util.LoggedEither._

import scalaz.std.set._
/**
  * Created by Loïc Girault on 04/05/16.
  */
object Remove{
  remove =>

  def concreteNode
  ( g : DependencyGraph,
    nid : NodeId
  ) : LoggedTG = remove.concreteNode(g, g getConcreteNode nid)

  def concreteNode
  ( g : DependencyGraph,
    n : ConcreteNode
  ) : LoggedTG = {
    val graph = g.comment(s"Remove($n)")
    for {
      g1 <- graph.content(n.id).map(graph.getConcreteNode).
        foldLoggedEither(graph)(remove.concreteNode)

      g2 <-
      if (g1.usersOf(n.id).nonEmpty)
        LoggedError("Cannot remove a used node")
      else {
        val g00 =
          n.kind.kindType match {
            case Parameter | TypeVariableKT => g1.removeEdge(ContainsParam(g1.container(n.id).get, n.id))
            case _ => g1.removeEdge(Contains(g1.container(n.id).get, n.id))
          }

        val g01 = graph.directSuperTypes(n.id).foldLeft(g00) {
          (g, supId) => g.removeIsa(n.id, supId)
        }
        val g02 = graph.usedByExcludingTypeUse(n.id).foldLeft(g01) {
          (g, usedId) =>
            //getUsesEdge needed to recover accessKind
            g.removeEdge(Uses(n.id, usedId))
        }
        val g03 = remove.bindingsInvolving(g02.removeType(n.id), n)
        val g04 = remove.typeUseConstraintInvolving(g03, n)
        LoggedSuccess(g04.removeConcreteNode(n))
      }

    } yield g2
  }

  def typeUseConstraintInvolving(g : DependencyGraph, n : ConcreteNode) : DependencyGraph = {
    val tcs = g.typeConstraintsIterator.filter {
      case ((user, used), ct) =>
        user == n.id || used == n.id
    }
    tcs.foldLeft(g){
      case (g0, (use, tuc)) => g0.removeTypeUsesConstraint(use, tuc)
    }
  }
  def bindingsInvolving(g : DependencyGraph, n : ConcreteNode) : DependencyGraph = {
    val g1 = n.kind.kindType match {
      case TypeDecl | TypeVariableKT =>
        g.typeUses2typeMemberUses.foldLeft(g) {
          case (g0, (tUses, typeMemberUses)) if tUses.used == n.id =>
            typeMemberUses.foldLeft(g0) { _.removeBinding(tUses, _)}
          case (g0, _) => g0
        }

      case NameSpace  => g // no type dependencies involved when removing namespaces
      case InstanceValueDecl
           | StaticValueDecl
           | TypeConstructor
           | Parameter
           | ValueDef =>
        g.typeUses2typeMemberUses.foldLeft(g) {
          case (g0, (tUses, typeMemberUses)) if tUses.user == n.id =>
            typeMemberUses.foldLeft(g0) { _.removeBinding(tUses, _)}
          case (g0, _) => g0
        }
      case UnknownKindType => error("removeBindingsInvolving UnknownKindType")

    }
    n.kind.kindType match {
      case ValueDef =>
        g1.typeMemberUses2typeUses.foldLeft(g1) {
          case (g0, (tmUses, typeUses)) if tmUses.user == n.id =>
            typeUses.foldLeft(g0) {
              _.removeBinding(_, tmUses)
            }
          case (g0, _) => g0
        }
      case _ => g1
    }

  }
}
