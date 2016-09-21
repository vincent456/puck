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

package puck.javaGraph.transformations

import puck.graph._
import puck.graph.transformations.MergeMatcher
import puck.javaGraph.nodeKind.{Interface, AbstractMethod}

object InterfaceMergeMatcher {

  implicit def mergeMatcher(n : ConcreteNode): MergeMatcher = n.kind match {
    case Interface => new InterfaceMergeMatcher(n)
    case AbstractMethod => new AbstractMethodMergeMatcher(n)
    case _ => new MergeMatcher(){
      override val node = n
      override def canBeMergedInto(other : ConcreteNode, styp : Option[Type], graph : DependencyGraph) = false
    }
  }

  def findMergingCandidateIn(g : DependencyGraph, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId] =
    g.styp(method.id) match {
      case None => throw new DGError("Method must have a type")
      case Some(t) =>
        val selfMappedT = Some(t.changeNamedType(g.container(method.id).get, interface.id))
        g.content(interface.id).find { ncId =>
          method.canBeMergedInto(g.getConcreteNode(ncId), selfMappedT, g)
        }
    }
}

import InterfaceMergeMatcher._


class InterfaceMergeMatcher(val node : ConcreteNode) extends MergeMatcher {



  override def canBeMergedInto(other : ConcreteNode, styp : Option[Type], graph : DependencyGraph): Boolean =
    super.canBeMergedInto(other, styp, graph) &&
      areMergingCandidates(node.id, other.id, graph)

  def areMergingCandidates(interface1 : NodeId, interface2: NodeId, g : DependencyGraph): Boolean = {

    def hasMatchingMethod(absmId : NodeId) = {
      val absm = g.getConcreteNode(absmId)
      absm.kind match {
        case AbstractMethod => findMergingCandidateIn(g, absm, g.getConcreteNode(interface2)).isDefined
        case _ =>
//          g.logger.writeln("searching for merging candidate "+
//            s"interface ${showDG[NodeId](g).shows(interface1)} contains ${showDG[NodeId](g).shows(absmId)}\n")
          true
      }
    }


    //the two interface are structurally compatible to merge
    g.content(interface2).size >= g.content(interface1).size &&
      (g.content(interface1) forall hasMatchingMethod) &&
      (g.content(interface2).size == g.content(interface1).size ||
        { g.directSubTypesId(interface1).forall(g.isa_*(_, interface2))
          //TODO structual type check
          /*val missingMethodsInThis =
            otherItc.content.filterNot{hasMatchingMethodIn(this)}*/
        }) ||
      //the two interfaces introduced an uneeded level of indirection
      g.isa(interface1, interface2) &&
        g.directSubTypesId(interface1).forall(g.isa_*(_,interface2))

  }
}

class FieldMergeMatcher(val node : ConcreteNode) extends MergeMatcher

class AbstractMethodMergeMatcher(val node : ConcreteNode) extends MergeMatcher {
  override def canBeMergedInto(other : ConcreteNode, styp : Option[Type], graph : DependencyGraph) : Boolean =
    super.canBeMergedInto(other, styp, graph) && other.name == node.name
}