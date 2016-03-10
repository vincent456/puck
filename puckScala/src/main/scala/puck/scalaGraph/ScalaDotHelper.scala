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

package puck.scalaGraph

import puck.graph.{NodeId, ConcreteNode, DependencyGraph, DGNode}
import puck.graph.io.{VisibilitySet, DotHelper}
import nodeKind._
import VisibilitySet._

object ScalaDotHelper extends DotHelper {
  override def isDotSubgraph(n: DGNode): Boolean =
    n.kind == Package

  override def namePrefix(n: DGNode): String =  n.kind match {
    case Package => "&lt;&lt;package&gt;&gt; "
    case Trait => "&lt;&lt;trait&gt;&gt; "
    case Object
      | PackageObject => "&lt;&lt;object&gt;&gt; "
    case _ => ""
  }

  override def splitDotClassContent(graph : DependencyGraph, n: NodeId, visibility : VisibilitySet.T) = {
    graph.content(n).foldLeft( (Seq[NodeId](), Seq[NodeId](), Seq[NodeId](), Seq[NodeId]()) ){
      ( lists : (Seq[NodeId], Seq[NodeId], Seq[NodeId] , Seq[NodeId]), n : NodeId ) =>
        if(visibility.isHidden(n)) lists
        else {
          val (fds, cts, mts, cls) = lists
          val kind = graph.getConcreteNode(n).kind
          kind match {
            case Trait | Class | Object | PackageObject => (fds, cts, mts, n +: cls)
            case Var | Val => (n +: fds, cts, mts, cls)
            case Def => (fds, cts, n +: mts, cls)

            case _ => throw new Error(kind + " : wrong NodeKind contained by a class")
          }
        }
    }
  }

  override def isDotClass(n : DGNode): Boolean = n.kind match {
    case Class | Trait | Object | PackageObject => true
    case _ => false
  }

  override def fillColor(n: DGNode): String = {
    def aux(cn : ConcreteNode) : String = cn.kind match {
      case Package => "#FF9933" //Orange
      case Trait => "#FFFF99" // Light yellow
      case Class => "#FFFF33" //Yellow
      case Object | PackageObject => "#FFFF22" //Yellow
      case Def | Var | Val => "#FFFFFF" //White
      case _ => throw new Error("Unknown ScalaNodeKind")
    }
    n mapConcrete(aux, "#00FF00")
  }
}
