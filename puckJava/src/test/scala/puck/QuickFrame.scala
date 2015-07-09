package puck

import java.io.{PipedInputStream, PipedOutputStream}
import javax.swing.{JFrame, WindowConstants}

import org.apache.batik.swing.JSVGCanvas
import puck.graph.DependencyGraph
import puck.graph.io.{Svg, FilesHandler, VisibilitySet, PrintingOptions}
import puck.gui.svg.SVGController
import puck.javaGraph.JavaDotHelper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object QuickFrame {

  def apply(graph : DependencyGraph, title : String = "QuickFrame") = {
    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    val opts = PrintingOptions(VisibilitySet.allVisible(graph), printId=true, printSignatures= true)

    Future {
      val canvas = new JSVGCanvas()
      canvas.setDocument(SVGController.documentFromStream(pipedInput))
      val imgframe = new JFrame(title)
      imgframe.add(canvas)
      imgframe.setVisible(true)
      imgframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    }

    FilesHandler.makeImage(None, JavaDotHelper, "")(graph, opts, Some(pipedOutput), Svg)()

  }
}
