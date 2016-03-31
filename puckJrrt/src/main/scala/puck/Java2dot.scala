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

package puck

import java.io.File

import org.extendj.ast.JavaJastAddDG2AST
import puck.config.ConfigParser
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.VisibilitySet._
import puck.graph.io._
import puck.graph.{DependencyGraph, NodeId}
import puck.javaGraph.JavaDotHelper
import puck.util.PuckSystemLogger

object Java2dot {

  def quickDot
  ( outFileName : String,
    dg : DependencyGraph,
    scm : Option[ConstraintsMaps],
    fullName2id : Map[String, NodeId]) : Unit = {

    val vis =
      VisibilitySet.allVisible(dg)
        .setVisibility(dg.subTree(fullName2id("java")), Hidden)
        .setVisibility(dg.subTree(fullName2id("@primitive")), Hidden)

    val options = PrintingOptions(vis,
      printId = false,
      printSignatures = true,
      selectedUse = None,
      printTypeUses = false)
    DotPrinter.genDotFile(dg, JavaDotHelper, scm, options, outFileName)
  }

  def main (args: Array[String]) : Unit = {

    val fn =
      if(args.isEmpty) "puck.xml"
      else args.head

    implicit val logger = new PuckSystemLogger(_ => true)

    val p = new Project(ConfigParser(new File(fn)), JavaJastAddDG2AST)
    val dg2ast = p.loadGraph()
    val scm = p.parseConstraints(dg2ast)

    quickDot("out", dg2ast.initialGraph, scm, dg2ast.nodesByName)
    logger writeln "done"
  }
}
