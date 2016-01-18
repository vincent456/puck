package puck.gui.svg

import java.awt.{Container, Dimension, BorderLayout}
import java.io.InputStream
import javax.swing._


import puck.actions.AddNodeAction
import puck.graph.{DependencyGraph, NameSpace, GraphUtils}
import puck.graph.io.PrintingOptions
import puck.gui._

import scala.swing.{Publisher, Label}

class SVGConsole
( val lines: Int = 10,
  val charPerLine: Int = 50) {

  private val selection: Label = new Label

  private val weight : Label = new Label

  val panel0 = new PuckConsolePanel(){
    selection +=: contents
    weight +=: contents
    textArea.rows = lines
    textArea.columns = charPerLine
  }

  def panel = panel0.peer
  def textArea = panel0.textArea

  private[svg] def displaySelection(node: String) : Unit = {
    if (node.length > 0) selection.text = "Selection : " + node
    else selection.text = "No selection"
  }

  private[svg] def displayWeight(w: Double) : Unit = {
    weight.text = "Selection : " + w
  }

  def appendText(txt: String) : Unit = {
    textArea.peer.append(txt + "\n")
  }
}


class SVGFrameMenu
( private val controller: SVGController,
  printingOptionsControl: PrintingOptionsControl
  ) extends JPanel {

  implicit def graph : DependencyGraph = controller.graphStack.graph
  implicit val graphUtils : GraphUtils = controller.graphUtils

  addVisibilityCheckBoxesToMenu(controller.printingOptionsControl.printingOptions)

  val buttonsWrapper = new JPanel()
  val undoRedoReset = PuckEvents.addUndoRedoButton(buttonsWrapper, controller)

  import controller.genControl.filesHandler.workingDirectory
  PuckEvents.addLoadSaveButton(buttonsWrapper, controller, workingDirectory)

  val hbox = new JPanel()
  hbox.setLayout(new BoxLayout(hbox, BoxLayout.Y_AXIS))
  hbox add buttonsWrapper
  hbox add controller.console.panel

  this add hbox


  addHboxButtons()


  private def addVisibilityCheckBoxesToMenu(printingOptions: PrintingOptions) : Unit = {
    val hbox = new JPanel()
    hbox.setLayout(new BoxLayout(hbox, BoxLayout.Y_AXIS))
    add(hbox)
    PuckEvents.addVisibilityCheckBoxes(hbox, controller, printingOptions)
    ()
  }



  private def addHboxButtons() : Unit = {
    val hbox = new JPanel()
    hbox.setLayout(new BoxLayout(hbox, BoxLayout.Y_AXIS))
    add(hbox)

    hbox add jbutton("Show recording") {
      _ => controller.printRecording()
    }

    val testCommutativityCB = puck.gui.svg.checkBox("Test commutativity",
      initiallySelected = false) {
      _ => ()
      }

    hbox add testCommutativityCB

    hbox add jbutton("Apply") {
      _ => controller publish GenCode(testCommutativityCB.isSelected)
    }

    import controller.graphUtils.nodeKindKnowledge.kindOfKindType
    assert(kindOfKindType(NameSpace).size == 1)

    hbox add new JButton(
      new AddNodeAction(controller, graph.root, kindOfKindType(NameSpace).head))


    hbox add jbutton("Show top level packages") {
      _ => printingOptionsControl.focusExpand(graph, graph.rootId, focus= false, expand = true)
    }

    val _ = hbox add jbutton("Hide type relationship") {
      _ => printingOptionsControl.selectedEdgeForTypePrinting = None
    }
  }
}

class SVGFrame
( stream: InputStream,
  control : PuckControl) extends JFrame {

    val pan = new SVGPanel(control)
    this.setContentPane(pan)

    this.setVisible(true)
      this.setMinimumSize(new Dimension(640, 480))
      this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      this.revalidate()
}

class SVGPanel( control : PuckControl)
  extends JPanel with Publisher{



  val console: SVGConsole = new SVGConsole()
  setLayout(new BorderLayout)

  val controller: SVGController = new SVGController(control, this)

  val canvas = new PUCKSVGCanvas(new SVGCanvasListener(this, controller))

  private val menu: SVGFrameMenu = new SVGFrameMenu(controller, controller.printingOptionsControl)

  reactions += {
    case urs : UndoRedoStatus =>
      menu.undoRedoReset(urs)
  }

  val centerPane = new JSplitPane()
  centerPane.setLeftComponent(canvas)
  centerPane.setRightComponent(new JPanel())
  centerPane.setResizeWeight(0.75)

  controller.displayGraph()


  this.add(centerPane, BorderLayout.CENTER)
  this.add(new JScrollPane(menu), BorderLayout.SOUTH)

}
