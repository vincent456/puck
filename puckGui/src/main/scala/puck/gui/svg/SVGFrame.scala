package puck.gui.svg

import java.awt.event.ActionEvent
import java.awt.{Container, Dimension, BorderLayout}
import java.io.{File, InputStream}
import javax.swing._

import puck.graph.{NameSpace, GraphUtils}
import puck.graph.io.{DG2AST, FilesHandler, PrintingOptions}
import puck.gui.svg.actions.AddNodeAction
import puck.gui.PuckConsolePanel

import scala.swing.Label

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
    textArea.append(txt + "\n")
  }
}


class SVGFrameMenu
( private val controller: SVGController
  ) extends JPanel {

  addVisibilityCheckBoxesToMenu()

  val buttonsWrapper = new JPanel()
  val (undoAllButton, undoButton, redoButton) = addUndoRedoButton(buttonsWrapper)

  addLoadSaveButton(buttonsWrapper)

  val hbox = new JPanel()
  hbox.setLayout(new BoxLayout(hbox, BoxLayout.Y_AXIS))
  hbox add buttonsWrapper
  hbox add controller.console.panel

  this add hbox


  addHboxButtons()


  def addUndoRedoButton(c : Container) : (JButton, JButton, JButton) = {
    val undoAllButton = jbutton("Undo all") {
      _ => controller.undoAll()
    }
    undoAllButton.setEnabled(false)
    c add undoAllButton

    val undoButton = jbutton("Undo") {
      _ => controller.undo()
    }
    undoButton.setEnabled(false)
    c add undoButton
    val redoButton = jbutton("Redo") {
      _ => controller.redo()
    }
    redoButton.setEnabled(false)
    c add redoButton

    (undoAllButton, undoButton, redoButton)
  }

  private def addLoadSaveButton(c : Container) : Unit = {
    import controller.filesHandler.workingDirectory
    c add jbutton("Save") {
      _ =>
        saveFile(workingDirectory, this) match {
          case None => controller.console.appendText("no file selected")
          case Some(f) =>  controller.saveRecordOnFile(f)
        }
    }
    c add jbutton("Load") {
      _ =>
        openFile(workingDirectory, this) match {
          case None => controller.console.appendText("no file selected")
          case Some(f) => controller.loadRecord(f)
        }
    } ; ()
  }

  private def addVisibilityCheckBoxesToMenu() : Unit = {
    val hbox = new JPanel()
    hbox.setLayout(new BoxLayout(hbox, BoxLayout.Y_AXIS))
    add(hbox)

    //    val inputNode = new JTextField()
    //    inputNode.setMinimumSize(new Dimension(50, 35))
    //    val getNodeName = new JButton(abstractAction("See node Name") {
    //      _ =>
    //        val id = inputNode.getText.toInt
    //        println(s"$id - ${controller.graph.fullName(id)}")
    //    })
    //    hbox.add(inputNode)
    //    hbox.add(getNodeName)

    def addCheckBox(name: String, initiallySelected : Boolean)(f: Boolean => Unit) =
      hbox add checkBox(name,initiallySelected)(f)
//    def addCheckBox(name: String, initiallySelected : Boolean)(f: Boolean => Unit) : JCheckBox = {
//      val checkBox: JCheckBox = new JCheckBox
//      checkBox.setSelected(initiallySelected)
//      checkBox.setAction(new AbstractAction(name) {
//        def actionPerformed(e: ActionEvent) : Unit = f(checkBox.isSelected)
//      })
//      hbox add checkBox
//      checkBox
//    }

    addCheckBox("Show signatures",
      controller.printingOptions.printSignatures) {
      controller.setSignatureVisible
    }

    addCheckBox("Show ids",
      controller.printingOptions.printId) {
      controller.setIdVisible
    }
    addCheckBox("Show Virtual Edges",
      controller.printingOptions.printVirtualEdges) {
      controller.setVirtualEdgesVisible
    }
    addCheckBox("Concrete Uses/Virtual Edge",
      controller.printingOptions.printConcreteUsesPerVirtualEdges) {
      controller.setConcreteUsesPerVirtualEdges
    }
    addCheckBox("Show RedOnly",
      controller.printingOptions.redOnly) {
      controller.setRedEdgesOnly
    }
    ()
  }



  private def addHboxButtons() : Unit = {
    val hbox = new JPanel()
    hbox.setLayout(new BoxLayout(hbox, BoxLayout.Y_AXIS))
    add(hbox)

    hbox add jbutton("Show recording") {
      _ => controller.printRecording()
    }
    //    hbox add jbutton("Show abstractions") {
    //      _ => controller.printAbstractions()
    //    }
    val testCommutativityCB = puck.gui.svg.checkBox("Test commutativity",
      initiallySelected = false) {
      _ => ()
      }

    hbox add testCommutativityCB

    hbox add   jbutton("Apply") {
      _ => controller.deleteOutDirAndapplyOnCode()
        if(testCommutativityCB.isSelected)
          controller.compareOutputGraph()
    }

    import controller.graphUtils.nodeKindKnowledge.kindOfKindType
    assert(kindOfKindType(NameSpace).size == 1)

    hbox add new JButton(
      new AddNodeAction(controller.graph.root, controller, kindOfKindType(NameSpace).head))


    hbox add jbutton("Show top level packages") {
      _ => controller.focusExpand(controller.graph.rootId, focus= false, expand = true)
    }

    val _ = hbox add jbutton("Hide type relationship") {
      _ => controller.setSelectedEdgeForTypePrinting(None)
    }
  }



}


class SVGFrame
( stream: InputStream,
  opts: PrintingOptions,
  filesHandler : FilesHandler,
  graphUtils : GraphUtils,
  dg2ast : DG2AST
  ) extends JFrame with StackListener {

  def update(svgController: SVGController) : Unit = {
    menu.undoAllButton.setEnabled(svgController.canUndo)
    menu.undoButton.setEnabled(svgController.canUndo)
    menu.redoButton.setEnabled(svgController.canRedo)
  }

  val console: SVGConsole = new SVGConsole()
  setLayout(new BorderLayout)
  val panel : SVGPanel =
    new SVGPanel(SVGController.documentFromStream(stream))

  setVisible(true)
  private val controller: SVGController =
    SVGController(
      filesHandler, graphUtils, dg2ast,
          opts, this)
  panel.controller = controller
  private val menu: SVGFrameMenu = new SVGFrameMenu(controller)
  controller.registerAsStackListeners(this)


  val centerPane = new JSplitPane()
  centerPane.setLeftComponent(panel)
  centerPane.setRightComponent(new JPanel())
  centerPane.setResizeWeight(0.75)

  this.add(centerPane, BorderLayout.CENTER)
  this.add(new JScrollPane(menu), BorderLayout.SOUTH)



  this.setVisible(true)
  this.setMinimumSize(new Dimension(640, 480))
  this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)







}
