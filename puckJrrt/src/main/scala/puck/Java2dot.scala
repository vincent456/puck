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

import java.io.{File, FileReader}

import puck.graph.constraints.{ConstraintsMaps, ConstraintsParser}
import puck.graph.io.VisibilitySet._
import puck.graph.io._
import puck.graph.{DependencyGraph, NodeId}
import puck.jastadd.CompileHelper
import puck.javaGraph.JavaDotHelper

object Java2dot {

  def quickDot
  ( outFileName : String,
    dg : DependencyGraph,
    cm : ConstraintsMaps,
    fullName2id : Map[String, NodeId]) : Unit = {

    val vis =
      VisibilitySet.allVisible(dg)
        .setVisibility(dg.subTree(fullName2id("java")), Hidden)
        .setVisibility(dg.subTree(fullName2id("@primitive")), Hidden)

    val options = PrintingOptions(vis, printId = true, printSignatures = false, selectedUse = None)
    DotPrinter.genDotFile(dg, JavaDotHelper, Some(cm), options, outFileName)
  }

  def main (args: Array[String]) : Unit = {

    val outFileName = args.head
    val decouple = new File(args.tail.head)
    val srcs = args.tail.tail.toList

    val (_, dg , _, fullName2id, _) =
      CompileHelper.compileSrcsAndbuildGraph(sources = srcs, sourcepaths = List(), jars = List(), bootJars = List(), decouple = Some(decouple))

    val cm = ConstraintsParser(fullName2id, new FileReader(decouple))

    quickDot(outFileName, dg, cm, fullName2id)
  }
}
