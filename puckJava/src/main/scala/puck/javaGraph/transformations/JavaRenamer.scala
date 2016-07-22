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
import puck.graph.transformations.rules.Renamer
import puck.javaGraph.nodeKind.{MethodKind, TypeKind, Constructor}

object JavaRenamer extends Renamer{

  override def apply
  ( graph : DependencyGraph,
    id : NodeId,
    newName : String) : DependencyGraph = {

    import puck.graph.ShowDG._
    val g = graph.comment(s"Rename(g, ${(graph, id).shows}, $newName)")
    val n = g.getNode(id)

    def dual = n.kind.kindType match {
        case TypeConstructor =>
          Set(g.container(id).get)
        case TypeDecl =>
          g.content(id).filter{nid => g.kindType(nid) == TypeConstructor}
        case _ => sys.error("Cannot happen")
      }

    def methodHierarchy : Set[NodeId] = {
      def aux(border : Set[NodeId], hierarchy : Set[NodeId]) : Set[NodeId] =
        if(border.isEmpty) hierarchy
        else {
          val nid = border.head
          val abs = g.abstractions(nid).filter{
            case AccessAbstraction(_, SupertypeAbstraction) => true
            case _ => false
          }.map(_.asInstanceOf[AccessAbstraction].nodeId)

          val subClasses = g.subTypes(g.container(nid).get)

          val allAbs = subClasses.foldLeft(abs){
            (acc, classId) =>
              g.content(classId).filter {
                subMethId => g.isAbstraction(subMethId, nid, SupertypeAbstraction)
              } union acc
          }

          val newBorder = allAbs.foldLeft(Set[NodeId]()){
            (border0, absId) =>
              if(hierarchy contains absId) border0
              else border0 + absId
          } union border.tail
          aux(newBorder, hierarchy + border.head)
        }

      aux(Set(id), Set())
    }


    n.kind match {
      case Constructor
        | _ : TypeKind => dual.foldLeft(g.setName(id, newName)){
        (g0, nid) => g0.setName(nid, newName)
      }
      case _ : MethodKind =>
        methodHierarchy.foldLeft(g){
          (g0, nid) => g0.setName(nid, newName)
        }

      case _ => super.apply(g, id, newName)
    }
  }
}
