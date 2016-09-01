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

package puck.javaGraph
package transformations

import nodeKind.Interface
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.MergeMatcherInstances
import puck.graph.transformations.rules.MergingCandidatesFinder
import puck.graph._

object JavaTransformationHelper extends MergingCandidatesFinder {

  def mergeMatcherInstances : MergeMatcherInstances =
    JavaMergeMatcherInstances

  override def find(g : DependencyGraph, node : ConcreteNode)(implicit constraints: ConstraintsMaps) : Option[ConcreteNode] = {

    val nid = node.id
    node.kind match {
      case Interface if g.content(nid).nonEmpty =>
        g.concreteNodes.find { other =>
          node.canBeMergedInto(other, None, g) &&
            g.usersOfExcludingTypeUse(nid).forall(!constraints.isForbidden(g, _,other.id)) &&
            g.usedByExcludingTypeUse(nid).forall( !constraints.isForbidden(g, other.id, _) )
        }
      case _ => None
    }

  }
  def findIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId] =
    (method.kind.kindType, interface.kind.kindType) match {
      case (InstanceValue, TypeDecl) => InterfaceMergeMatcher.findMergingCandidateIn(g, method, interface)
      case _ => None
    }

}
