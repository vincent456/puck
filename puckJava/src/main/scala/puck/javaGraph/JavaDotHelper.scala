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

import puck.graph.{ConcreteNode, NodeId, DependencyGraph, DGNode}
import puck.graph.io.DotHelper
import puck.javaGraph.nodeKind._

object JavaDotHelper extends DotHelper{

  override def isDotSubgraph(n: DGNode): Boolean =
    n.kind == Package

  override def namePrefix(n: DGNode): String =  n.kind match {
      case Package => "&lt;&lt;package&gt;&gt; "
      case Interface => "&lt;&lt;interface&gt;&gt; "
      case _ => ""
    }

  override def splitByKind(graph : DependencyGraph, ns: Seq[NodeId]) = {
    val init : Seq[Seq[NodeId]] = Seq(Seq(), Seq(), Seq(), Seq(), Seq())
    ns.foldLeft( init ){
      case (s @ Seq(fds, cts, mts, cls, tvs), n) =>
          val kind = graph.getConcreteNode(n).kind
          kind match {
            //case TypeVariable => s
            case _ : TypeKind => Seq(fds, cts, mts, n +: cls, tvs)
            case Constructor => Seq(fds, n +: cts, mts, cls, tvs)
            case Field
            | StaticField
            | EnumConstant => Seq(n +: fds, cts, mts, cls, tvs)
            case _ : MethodKind
            | StaticMethod => Seq(fds, cts, n +: mts, cls, tvs)
            case _ => throw new Error(kind + " : wrong NodeKind contained by a class")
          }
        }

  }

  override def isDotClass(n : DGNode): Boolean = n.kind match {
      case Class | Interface | Primitive => true
      case _ => false
    }

  override def fillColor(n: DGNode): String = {
    def aux(cn : ConcreteNode) : String = cn.kind match {
        case Package => "#FF9933" //Orange
        case _ : TypeKind.InterfaceLike => "#FFFF99" // Light yellow
        case _ : TypeKind.ClassLike | Constructor => "#FFFF33" //Yellow
        case Method | Field | EnumConstant => "#FFFFFF" //White
        case Literal => "#CCFFCC" //Very Light green
        case Primitive => "#FFFFFF"
        case TypeVariable => "#009900"
        case k => throw new Error(s"$k : Unknown JavaNodeKind")
      }
    n mapConcrete(aux, "#00FF00")
  }
}