package puck.gui.svg

import java.awt.event.ActionEvent
import java.awt.{Dimension, BorderLayout}
import java.io.{File, InputStream}
import javax.swing._

import puck.graph.DependencyGraph
import puck.graph.io.PrintingOptions
import puck.gui.PuckControl

class SVGConsole
( val lines: Int = 10,
  val charPerLine: Int = 50) {





  val panel: JPanel  = new JPanel
  private val selection: JLabel = new JLabel
  private val textArea: JTextArea = new JTextArea(lines, charPerLine)

  panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS))
  panel.add(selection)
  panel.add(textArea)
  textArea.setEditable(false)

  private[svg] def displaySelection(node: String) : Unit = {
    if (node.length > 0) selection.setText("Selection : " + node)
    else selection.setText("No selection")
  }

  def appendText(txt: String) : Unit = {
    textArea.append(txt + "\n")
  }
}

class SVGFrame
( stream: InputStream,
  g: DependencyGraph,
  opts: PrintingOptions,
  control: PuckControl
  ) extends JFrame with StackListener {

  def update(svgController: SVGController) : Unit = {
    undoButton.setEnabled(svgController.canUndo)
    redoButton.setEnabled(svgController.canRedo)
  }

  private val console: SVGConsole = new SVGConsole()
  setLayout(new BorderLayout)
  val panel : SVGPanel = new SVGPanel(SVGController.documentFromStream(stream))
  setVisible(true)

  private val controller: SVGController = SVGController(control, g, opts, panel.canvas, console)
  panel.setController(controller)
  private val menu: JPanel = new JPanel()
  controller.registerAsStackListeners(this)

  addVisibilityCheckBoxesToMenu()

  private val undoButton = new JButton(abstractAction("Undo") {
    _ => controller.undo()
  })
  undoButton.setEnabled(false)
  menu.add(undoButton)
  private val redoButton = new JButton(abstractAction("Redo"){
    _ => controller.redo()
  })
  redoButton.setEnabled(false)
  menu.add(redoButton)

  addLoadSaveButton()

  menu.add(console.panel)

  def abstractAction(name:String)
                    (action : ActionEvent => Unit) : AbstractAction =
    new AbstractAction(name){
      def actionPerformed(e: ActionEvent) : Unit = action(e)

  }
  def addButtonToMenu(name:String)
            (action : ActionEvent => Unit) : Unit =
    menu.add( new JButton(abstractAction(name) {
      _ => controller.applyOnCode()

    }))

  addButtonToMenu("Apply") {
   _ => controller.applyOnCode()
  }

  this.add(panel, BorderLayout.CENTER)
  this.add(menu, BorderLayout.SOUTH)


  this.setVisible(true)
  this.setMinimumSize(new Dimension(640, 480))
  this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)


  private def chooseFile() : Option[File] = {
    val chooser = new JFileChooser()
    val returnVal: Int = chooser.showOpenDialog(SVGFrame.this)
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      Some(chooser.getSelectedFile)
    }
    else None
  }

  private def addLoadSaveButton() : Unit = {
    addButtonToMenu("Save") {
      _ =>
        chooseFile() match {
          case None => console.appendText("no file selected")
          case Some(f) =>  controller.saveRecordOnFile(f)
        }
      }
    addButtonToMenu("Load") {
      _ =>
        chooseFile() match {
          case None => console.appendText("no file selected")
          case Some(f) => controller.loadRecord(f)
        }
    }
  }

  private def addVisibilityCheckBoxesToMenu() : Unit = {
    val sigCheckBox: JCheckBox = new JCheckBox
    sigCheckBox.setAction(new AbstractAction("Show signatures") {
      def actionPerformed(e: ActionEvent) : Unit = {
        controller.setSignatureVisible(sigCheckBox.isSelected)
      }
    })
    menu.add(sigCheckBox)
    val idCheckBox: JCheckBox = new JCheckBox
    idCheckBox.setAction(abstractAction("Show ids"){
     _ => controller.setIdVisible(idCheckBox.isSelected)
      })

    menu.add(idCheckBox)
    val vEdgesCheckBox: JCheckBox = new JCheckBox
    vEdgesCheckBox.setAction(abstractAction("Show Virtual Edges"){
      _ =>
      controller.setVirtualEdgesVisible(vEdgesCheckBox.isSelected)
      })
    menu.add(vEdgesCheckBox)
    val redOnlyCheckBox: JCheckBox = new JCheckBox
    redOnlyCheckBox.setAction(abstractAction("Show RedOnly"){
      _ =>
      controller.setRedEdgesOnly(redOnlyCheckBox.isSelected)
      })
    menu.add(redOnlyCheckBox);()
  }


}
