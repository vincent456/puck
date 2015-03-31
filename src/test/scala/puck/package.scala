import java.io.{PipedOutputStream, PipedInputStream}
import javax.swing.{WindowConstants, JFrame}

import org.apache.batik.swing.JSVGCanvas
import puck.graph.DependencyGraph
import puck.graph.io.{Svg, VisibilitySet, PrintingOptions, FilesHandler}
import puck.gui.svg.SVGController
import puck.javaGraph.JavaDotHelper
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by lorilan on 22/02/15.
 */
package object puck {
  val testPath = "/home/lorilan/projects/constraintsSolver/src/test"
  val testExamplesPath = testPath + "/resources/examples"

  def quickFrame(graph : DependencyGraph) = {
    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)

    val opts = PrintingOptions(VisibilitySet.allVisible(graph), printId=true, printSignatures= true)

    Future {
      val canvas = new JSVGCanvas()
      canvas.setDocument(SVGController.documentFromStream(pipedInput))
      val imgframe = new JFrame("Test !")
      imgframe.add(canvas)
      imgframe.setVisible(true)
      imgframe.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    }
    FilesHandler.makeImage(None, JavaDotHelper, "")(graph, opts, Some(pipedOutput), Svg)()

  }}
