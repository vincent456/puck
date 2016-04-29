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

package puck.gui

import java.io.{PipedInputStream, PipedOutputStream, InputStream}

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.svg.SVGDocument
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.{Svg, DotPrinter, PrintingOptions}
import puck.graph.{GraphUtils, DependencyGraph}

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success}

/**
  * Created by Loïc Girault on 26/01/16.
  */
package object svg {
  def documentFromStream(stream: InputStream): SVGDocument = {
    val parser: String = XMLResourceDescriptor.getXMLParserClassName
    val factory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser)
    factory.createSVGDocument("", stream)
  }


  def documentFromGraphErrorMsgGen(consumer : String => Unit) : scala.util.Try[Int] => Unit ={
    case Success(0) => ()
    case Success(n) =>
      consumer("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
    case Failure(errMsg) =>
      consumer("Image creation failure : " + errMsg)
  }


  def documentFromGraph
  ( graph: DependencyGraph,
    graphUtils : GraphUtils,
    scm : Option[ConstraintsMaps],
    printingOptions: PrintingOptions,
    fail : Boolean = false)
  ( onDotConversionResult: scala.util.Try[Int] => Unit)
  ( onDocBuildingSuccess : PartialFunction[SVGDocument, Unit])
  ( implicit executor: ExecutionContext ) : Unit = {

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)
    val fdoc = Future {
      documentFromStream(pipedInput)
    }

    DotPrinter.genImage(graph, graphUtils.dotHelper, scm, printingOptions, Svg,
      pipedOutput)(onDotConversionResult)


    fdoc.onSuccess(onDocBuildingSuccess)
  }
}
