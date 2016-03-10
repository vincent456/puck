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

package puck.graph

import puck.graph.transformations.TransformationRules
import puck.util.LoggedEither._

import scalaz.std.set._

object Concretize {

  def apply(g : DependencyGraph, rules : TransformationRules) : LoggedTG =
    if(g.virtualNodes.isEmpty) LoggedSuccess(g)
    else {
      val vn = g.virtualNodes.head

      g.content(vn.id).foldLoggedEither(g){ (g0, cid) =>
        val usedElts : Set[NodeId] = Metrics.outgoingDependencies(g0, cid).map(_._2)
        val c = g0.getConcreteNode(cid)
        val potentialContainers = g0.concreteNodes.filter(g0.canContain(_, c))
        val pcWithCohesion = potentialContainers.foldLeft(List[(Double, NodeId)]()){ (l, pc) =>
          (Metrics.relativeCohesion(g0, usedElts, pc.id), pc.id) :: l
        }

        val best = pcWithCohesion.sortBy(_._1).head._2
        c.kind.kindType match {
          case TypeDecl => rules.move.staticDecl(g, cid, best)
          case _ => ???
        }

      } flatMap {
        g =>
        apply(g removeVirtualNode vn, rules)
      }

     }

}
