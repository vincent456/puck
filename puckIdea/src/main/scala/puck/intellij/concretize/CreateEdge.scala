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

package puck.intellij.concretize

import com.intellij.openapi.module.Module
import com.intellij.psi.PsiElementFactory
import puck.graph._
import puck.graph.transformations.{Edge, Regular, Transformation}
import puck.intellij.{TypedKindDeclHolder, PackageDummyWrapper, PsiNodeWrapper}
import puck.util.PuckLogger

/**
 * Created by Loïc Girault on 04/11/15.
 */
object CreateEdge {

  def unapply(t : Transformation) : Option[DGEdge] =
    t match {
      case Transformation(Regular, Edge(e)) => Some(e)
      case _ => None
    }

  def apply
  ( resultGraph : DependencyGraph,
    id2declMap: Map[NodeId, PsiNodeWrapper],
    edge : DGEdge)
  (implicit factory : PsiElementFactory,
     module: Module,
     logger : PuckLogger) : Unit =
    (edge.kind, id2declMap(edge.source), id2declMap(edge.target)) match {
    case (Contains, PackageDummyWrapper, tdh : TypedKindDeclHolder ) =>

      val clss = tdh.node
      clss.setName(resultGraph.fullName(edge.source)+ "." + clss.getName)
      //val d = FilenameIndex.getVirtualFilesByName(module.getProject, node.name, module.getModuleContentScope)

    case _ => ()
  }

}
