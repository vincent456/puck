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
import puck.graph.{DGBuildingError, DependencyGraph, NodeId}
import puck.util.{FileHelper, PuckFileLogger, PuckLogger}
import sbt.IO
import org.extendj.ast.{ASTNodeLink, JavaJastAddDG2AST, Program}
import FileHelper.FileOps
import puck.Project
import puck.config.ConfigParser





object ScenarioFactory {
  implicit val logger : PuckLogger = new PuckFileLogger(_ => true, new java.io.File("/tmp/comparisonLog"))

  def fromDirectory(path: String): ScenarioFactory = {
     new ScenarioFactory(FileHelper.findAllFiles(new File(path), ".java", Seq(new File(path) \ "out")):_*)
  }

  def fromConfigFile(f : File) : ScenarioFactory = {
    val dg2ast = new Project(ConfigParser(f),JavaJastAddDG2AST).loadGraph().asInstanceOf[JavaJastAddDG2AST]
    new ScenarioFactory(dg2ast)
  }

}

import ScenarioFactory.logger

case class ScenarioFactory
( program : Program,
  graph : DependencyGraph,
  initialRecord : Seq[Transformation],
  fullName2id : Map[String, NodeId],
  dg2astMap : Map[NodeId, ASTNodeLink]){

   def this(dg2ast : JavaJastAddDG2AST) =
    this(dg2ast.program,
      dg2ast.initialGraph,
      dg2ast.initialRecord,
      dg2ast.nodesByName,
      dg2ast.graph2ASTMap)

  def this(filePathOrCode : String) = this {
    if(filePathOrCode endsWith ".java" )
      JavaJastAddDG2AST.fromFiles(List(filePathOrCode), List(), List(), List())
    else
      JavaJastAddDG2AST.compile(filePathOrCode.stripMargin) match {
        case None => throw new DGBuildingError("Compilation error, no AST generated")
        case Some(p) => JavaJastAddDG2AST.buildGraph(p, null)
      }
  }


  def this(filePathOrCode : String*) = this {
    if(filePathOrCode.head endsWith ".java" )
      JavaJastAddDG2AST.fromFiles(filePathOrCode.toList, List(), List(), List())
    else
      JavaJastAddDG2AST.compile(filePathOrCode.toList map (_.stripMargin)) match {
        case None => throw new DGBuildingError("Compilation error, no AST generated")
        case Some(p) => JavaJastAddDG2AST.buildGraph(p, null)
      }
  }


  implicit val logger = ScenarioFactory.logger

  protected implicit def idOfFullName(fn : String) : NodeId = fullName2id apply fn


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
    fullName2id.toList sortBy(_._1) foreach println

  def printFullNamesSortedByKey() : Unit =
    fullName2id.toList map (_.swap) sortBy(_._1) foreach println


}
