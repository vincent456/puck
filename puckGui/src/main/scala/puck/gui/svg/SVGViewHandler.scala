package puck.gui.svg

import java.io.{File, FileWriter}
import javax.swing.JButton

import puck._
import puck.actions.AddNodeAction
import puck.graph.{NameSpace, GraphUtils, DependencyGraph}
import puck.graph.io.DotPrinter
import puck.gui.explorer.DGTreeIcons
import puck.gui.svg.actions.{SwingService, DefaultSwingService}
import puck.gui._

import scala.swing.SequentialContainer.Wrapper
import scala.swing._
import scala.util.{Failure, Success}

/**
  * Created by lorilan on 26/01/16.
  */
class SVGViewHandler
(mainPanel : PuckMainPanel)
  extends ViewHandler with Publisher {
  import mainPanel.control



  def switchView(mainPanel: PuckMainPanel, treeIcons: DGTreeIcons) : Unit = {
    this deafTo mainPanel.control.Bus
    val tvh = new TreeViewHandler(mainPanel, treeIcons)
    mainPanel.viewHandler = tvh
    tvh.update(mainPanel.control.graph)
    mainPanel.revalidate()
  }

  this listenTo mainPanel.control.Bus
  reactions += {
    case GraphUpdate(graph) =>
      this.displayGraph(graph)

    case GraphFocus(graph, edge) =>
      this.displayGraph(graph)

    case PrintingOptionsUpdate =>
      this.displayGraph(control.graph)
  }


  import control.{graphUtils, logger, printingOptionsControl}
  val swingService : SwingService = DefaultSwingService
  import swingService._

  val svgController: SVGController = new SVGController(mainPanel.control, mainPanel.console)
  val canvas = PUCKSVGCanvas(svgController, swingService, mainPanel.treeIcons)
  mainPanel.upPanel.setGraphView(Component.wrap(canvas))


  def displayGraph(graph : DependencyGraph, recCall : Boolean = false): Unit =
    documentFromGraph(graph, graphUtils,
      printingOptionsControl.printingOptions) {
      res =>
        val smsg : Option[String] = res match {
          case Success(0) => None
          case Success(n) =>

            if (recCall)
              Some("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
            else {
              val tmpDir = System.getProperty("java.io.tmpdir")
              val f = new File(tmpDir + File.separator + "graph.dot")

              DotPrinter.genDot(graph, graphUtils.dotHelper,
                printingOptionsControl.printingOptions,
                new FileWriter(f))

              Some("error during SVG production dot can be found at " + f.getAbsolutePath /*+
                "\nretry with top level package visibility only"*/)

            }

          case Failure(errMsg) =>
            Some("Image creation failure : " + errMsg)

        }
        smsg foreach (msg => swingInvokeLater(() => logger writeln msg))
    }{
      case doc =>
        swingInvokeLater(() => canvas.setDocument(doc))
    }

  displayGraph(control.graph)

  mainPanel.downPanel.leftComponent = new SVGMenu(svgController)

}

object SVGMenu {
  def addVisibilityCheckBoxes(c : Wrapper,
                              control: PrintingOptionsControl) : Unit  = {
    def addCheckBox(n: String, initiallySelected: Boolean)(f: Boolean => Unit) = {
      c.contents += new CheckBox(n) {
            selected = initiallySelected
            action = new Action(n) {
              def apply() = f(selected)
            }
          }
    }
    val printingOptions = control.printingOptions
    addCheckBox ("Show signatures",
      printingOptions.printSignatures) {
      b => control.signatureVisible = b
    }

    addCheckBox ("Show ids",
      printingOptions.printId) {
      b => control.idVisible = b
    }
    addCheckBox ("Show Virtual Edges",
      printingOptions.printVirtualEdges) {
      b => control.virtualEdgesVisible = b
    }
    addCheckBox ("Concrete Uses/Virtual Edge",
      printingOptions.printConcreteUsesPerVirtualEdges) {
      b =>  control.concreteUsesPerVirtualEdges = b
    }
    ignore(addCheckBox ("Show RedOnly",
      printingOptions.redOnly) {
      b => control.redEdgesOnly = b
    })
  }
}


class SVGMenu
( private val controller: SVGController) extends BoxPanel(Orientation.Vertical) {

  implicit def graph : DependencyGraph = controller.graph
  implicit val graphUtils : GraphUtils = controller.graphUtils
  val printingOptionsControl = controller.printingOptionsControl


  SVGMenu.addVisibilityCheckBoxes(this, printingOptionsControl)


  import graphUtils.nodeKindKnowledge.kindOfKindType
  assert(kindOfKindType(NameSpace).size == 1)

  contents += Component.wrap(new JButton(new AddNodeAction(controller, graph.root, kindOfKindType(NameSpace).head)))

  contents += button("Show top level packages"){
    () => printingOptionsControl.
      focusExpand(graph, graph.rootId, focus= false, expand = true)
  }

  contents += button("Hide type relationship") {
    () => printingOptionsControl.selectedEdgeForTypePrinting = None
  }


}