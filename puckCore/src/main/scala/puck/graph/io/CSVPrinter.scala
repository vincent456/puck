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

package puck.graph.io

import puck.error
import java.io.{File, FileWriter}

import puck.graph.{DGNode, DependencyGraph, InstanceValueDecl, NodeIdP, Parameter, StaticValueDecl, TypeConstructor}

object CSVPrinter{

  val bl = System getProperty "line.separator"

  def apply(graph : DependencyGraph, f : File, sep : String = ";") : Unit =
    if(!f.isDirectory)
      error("CSVPrinter(graph, f) error : f must be a directory")
    else {

      import puck.graph.ShowDG._
      def sig(n : DGNode) : String =
        n.kind.kindType match {
          case TypeConstructor
               | InstanceValueDecl
               | StaticValueDecl
               | Parameter =>
            graph.structuredType(n.id) map (t => (graph, t).shows) getOrElse ""
          case _ => ""
        }

      import puck.util.FileHelper.FileOps

      val nodesWriter = new FileWriter(f \ "nodes.csv")
      nodesWriter write s"id\t$sep kind\t$sep name\t$sep qualified name\t$sep type$bl"
      graph.nodes.foreach (
        n =>
          nodesWriter write s"${n.id}\t$sep ${n.kind}\t$sep ${n.name}\t$sep ${graph fullName n.id}\t$sep ${sig(n)}$bl"
      )
      nodesWriter.close()

      val edgesWriter = new FileWriter(f \ "edges.csv")
      edgesWriter write s"source\t$sep target\t$sep label$bl"

      def printEdges(edges : List[NodeIdP], label : String) =
        edges foreach {
          case (source, target) =>
            edgesWriter write s"$source\t$sep $target\t$sep $label$bl"
        }

      printEdges(graph.containsList, "Contains")
      printEdges(graph.isaList, "Isa")
      printEdges(graph.usesList, "Uses")

      edgesWriter.close()
    }
}