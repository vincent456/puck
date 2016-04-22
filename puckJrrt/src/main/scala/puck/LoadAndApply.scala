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

import puck.graph.comparison.Mapping
import puck.graph.transformations.Recording
import puck.util.PuckSystemLogger
import sbt.IO
import jastadd._
import Recording.RecordingOps
object LoadAndApply {

  def main (args: Array[String]) : Unit = {

    val recFileName = args.head
    val recFile = new File(recFileName)
//    val recFile = new File(FrontVars.workspace + "/planB2.puck")
    val fh = JavaProject()
    implicit val logger = new PuckSystemLogger(_ => true)

    val dg2ast = fh.loadGraph()

    val r = Recording.load(recFile.getAbsolutePath, dg2ast.nodesByName  )
    val g = r.redo(dg2ast.initialGraph)

    val outDirectory = fh.outDirectory get

    if(outDirectory.exists())
      IO.delete(outDirectory)


    dg2ast(g)
    dg2ast.printCode(outDirectory)


    val fhout = JavaProject(outDirectory)
    val dg2astout = fhout.loadGraph()

    val gout = dg2astout.initialGraph

    try {
      if(Mapping.equals(g, gout)){
        println("ARE equals")
      }
      else {
        println("are NOT equals")
        import puck.graph.ShowDG._

//        import scalaz.syntax.show._
//        import puck.util.Debug.showNodeIndex
        println((g, g.nodesIndex).shows)
        println((gout, gout.nodesIndex).shows)

//        val mapping = Mapping.create(g, gout)
//
//        println(Mapping.mapCVM(mapping, g.edges.userMap))
//        println(gout.edges.userMap)
      }
    } catch {
      case e : PuckError => println(e.getMessage)
    }
  }

}
