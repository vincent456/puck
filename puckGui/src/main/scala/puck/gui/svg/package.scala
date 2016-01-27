package puck.gui

import java.io.{PipedInputStream, PipedOutputStream, InputStream}

import org.apache.batik.anim.dom.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import org.w3c.dom.svg.SVGDocument
import puck.graph.io.{Svg, DotPrinter, PrintingOptions}
import puck.graph.{GraphUtils, DependencyGraph}

import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Failure, Success}

/**
  * Created by lorilan on 26/01/16.
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

    DotPrinter.genImage(graph, graphUtils.dotHelper, printingOptions, Svg,
      pipedOutput)(onDotConversionResult)


    fdoc.onSuccess(onDocBuildingSuccess)
  }
}
