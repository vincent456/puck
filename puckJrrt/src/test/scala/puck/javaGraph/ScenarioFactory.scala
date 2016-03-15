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

import java.io.File

import puck.graph.transformations.Transformation
import puck.graph.{DependencyGraph, NodeId}
import puck.jastadd.CompileHelper
import puck.util.{FileHelper, PuckFileLogger, PuckLogger}
import sbt.IO
import org.extendj.ast.{ASTNodeLink, JavaJastAddDG2AST, Program}
import FileHelper.FileOps
object ScenarioFactory {
  def fromDirectory(path: String): ScenarioFactory = {
     new ScenarioFactory(FileHelper.findAllFiles(new File(path), ".java", Seq(new File(path) \ "out")):_*)
  }
}


case class ScenarioFactory
( program : Program,
  graph : DependencyGraph,
  initialRecord : Seq[Transformation],
  fullName2id : Map[String, NodeId],
  dg2astMap : Map[NodeId, ASTNodeLink]){

  def this(t : (Program,
                DependencyGraph,
                Seq[Transformation],
                Map[String, NodeId],
                Map[NodeId, ASTNodeLink])) =
        this(t._1, t._2, t._3, t._4, t._5)

  def this(filePath : String) =
    this(CompileHelper.compileSrcsAndbuildGraph(List(filePath), List(), List(), List()))
  def this(filesPath : String*) =
    this(CompileHelper.compileSrcsAndbuildGraph(filesPath.toList, List(), List(), List()))

  implicit var logger : PuckLogger = new PuckFileLogger(_ => true, new java.io.File("/tmp/comparisonLog"))
  def compare: (DependencyGraph, DependencyGraph) => Boolean =
    (g1, g2) => DependencyGraph.areEquivalent(initialRecord,g1,g2, logger)


  def applyChanges(g: DependencyGraph,
                   outDir : File) : Unit = {
    val dg2ast = new JavaJastAddDG2AST(program, graph, initialRecord, fullName2id, dg2astMap)

    dg2ast.apply(g)(new PuckFileLogger(_ => true, new File("/tmp/pucklog")))
    IO.delete(outDir)
    dg2ast.printCode(outDir)
  }
  def applyChangeAndMakeExample
  ( g: DependencyGraph,
    outDir : File) : ScenarioFactory = {
    applyChanges(g, outDir)

    val genSrc = FileHelper.findAllFiles(outDir, ".java", Seq())
    new ScenarioFactory(genSrc:_*)
  }

  def printFullNames() : Unit =
    fullName2id.keys.toList.sorted foreach println

  def printFullNamesSortedByKey() : Unit =
    fullName2id.toList map (_.swap) sortBy(_._1) foreach println
}
