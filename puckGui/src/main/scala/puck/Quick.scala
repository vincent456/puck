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

import java.awt.Dimension
import java.io.{FileWriter, PipedInputStream, PipedOutputStream}
import javax.swing.{JFrame, WindowConstants}

import org.apache.batik.swing.JSVGCanvas
import puck.graph.DependencyGraph
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Quick {

  def frame(graph : DependencyGraph, title : String = "QuickFrame", scm : Option[ConstraintsMaps] = None)
           (implicit dotHelper : DotHelper)= {
    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    val opts = PrintingOptions(VisibilitySet.allVisible(graph), printId=true, printSignatures= true)

    Future {
      val canvas = new JSVGCanvas()
      canvas.setDocument(gui.svg.documentFromStream(pipedInput))
      new JFrame(title) {
        setMinimumSize(new Dimension(1024,768))
        add(canvas)
        setVisible(true)
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      }
    }

    DotPrinter.genImage(graph, dotHelper, scm, opts, Svg, pipedOutput)()

  }

  def dot ( graph : DependencyGraph, path : String, scm : Option[ConstraintsMaps] = None)
          ( implicit dotHelper : DotHelper): Unit = {
    val opts = PrintingOptions(VisibilitySet.allVisible(graph), printId=true, printSignatures= true)
    DotPrinter.genDot(graph, dotHelper, scm, opts, new FileWriter(path))
  }
}
