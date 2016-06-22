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

import java.io.FileWriter

import puck.graph.{DependencyGraph, ShowDG}
import puck.graph.comparison.Mapping
import puck.graph.constraints.ConstraintsMaps
import puck.util.PuckLogger
import sbt.IO

/**
  * Created by Loïc Girault on 05/01/16.
  */
object ProjectDG2ASTControllerOps {


  def deleteOutDirAndApplyOnCode
  (dg2ast : DG2AST,
   project : Project,
   graph : DependencyGraph,
   constraints : Option[ConstraintsMaps])
  (implicit logger : PuckLogger) : Unit = {

    logger.writeln("Aplying recording on AST")
    dg2ast(graph)/*(new PuckFileLogger(_ => true, new File("/tmp/pucklog")))*/

    project.outDirectory match {
      case None => logger.writeln("no output directory : cannot print code")
      case Some(d) =>
        logger.writeln("Printing code")
        IO.delete(d)
        dg2ast.printCode(d)

        constraints foreach {
          cm =>
            logger.write("printing constraints ...")
            import puck.util.FileHelper.FileOps
            val decoupleName = project.decouple.map(_.getName) getOrElse "decouple.wld"
            val fw = new FileWriter(d \ decoupleName)
            import ShowDG._
            fw write (graph,cm).shows
            fw.close()
        }

    }

  }

  def compareOutputGraph
  (filesHandler : Project,
   graph : DependencyGraph)(implicit logger : PuckLogger) : Unit = {
    val outfh = filesHandler.fromOutDir
    logger.writeln("Loading output graph from code")
    val outdg2ast = outfh.loadGraph()
    logger.writeln("Comparing graphs ...")

    val res = if(Mapping.equals(graph, outdg2ast.initialGraph)) "EQUAL"
    else "NOT equal"

    logger.writeln(s"they are $res")
  }

}
