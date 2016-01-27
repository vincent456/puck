package puck.gui

import java.awt.Dimension
import java.io.File

import puck.graph.io.VisibilitySet
import scala.swing._
import scala.swing.SequentialContainer.Wrapper

class PuckInterfacePanel
( control : PuckControl
) extends BoxPanel(Orientation.Vertical)  {

  private val publisher = this
  control listenTo this
  this listenTo control

  val leftWidth = PuckMainPanel.width * 3/8
  val rightWidth = PuckMainPanel.width * 5/8
  val height = PuckMainPanel.height * 2/3



  def makeButton(title:String, tip: String)(act:() => Unit): Component =
    new Button() {
      tooltip = tip
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize

      action = new Action(title) {
        def apply(): Unit = {
          act()
        }
      }
    }.leftGlued


  preferredSize = new Dimension(leftWidth, height)

  import control.filesHandler

  def addAlwaysVisibleButtons(): Unit ={
    contents += makeButton("Settings", "To set graphviz dot path"){
      () => val frame = new SettingsFrame(filesHandler)
        frame.visible = true
    }

    contents += makeButton("Work space",
      "Select the root directory containing the java (up to 1.5) source code you want to analyse"){
      () => val fc = new FileChooser(filesHandler.srcDirectory !)
        fc.title = "What directory contains your application ?"
        fc.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        fc showDialog(null, "Select")
        val f: File = fc.selectedFile
        if( f != null && !(f == filesHandler.srcDirectory.!)) {
          filesHandler.setWorkingDirectory(f)
          publish(LoadCodeRequest)
        }
        val sf : Option[File]= filesHandler.srcDirectory.get
        val path = sf map (_.getAbsolutePath) getOrElse "No directory selected"
        this publish Log(s"Application directory :\n$path")
    }

    contents += makeButton("(Re)load code & constraints",
      "Load the selected source code and build the access graph"){
      () => publish(LoadCodeRequest)
    }
    contents += control.progressBar
  }

  addAlwaysVisibleButtons()


  def addUndoRedoButton(c : Wrapper) : Unit = {
    c.contents +=
      new Button(new Action("Undo all") {

        enabled = false

        def apply() = publisher publish UndoAll

        reactions += {
          case UndoRedoStatus(canUndo, _) =>
            enabled = canUndo
        }
        listenTo(control)
      })

    c.contents +=
      new Button(new Action("Undo") {

        enabled = false

        def apply() = publisher publish Undo

        reactions += {
          case UndoRedoStatus(canUndo, _) =>
            enabled = canUndo
        }
        listenTo(control)
      })

    c.contents +=
      new Button(new Action("Redo") {

        enabled = false

        def apply() = publisher publish Undo

        reactions += {
          case UndoRedoStatus(_, canRedo) =>
            enabled = canRedo
        }
        listenTo(control)
      })
  }

  def addLoadedGraphButtons(): Unit= {
      contents += makeButton("(Re)load constraints",
        "Decorate the graph with the constraints of the selected decouple file"){
        () => publisher publish LoadConstraintRequest
      }
      contents += makeButton("Show constraints",
        "Show the constraints the graph has to satisfy"){
        () => publisher publish ConstraintDisplayRequest(control.graph)
      }

      val p = new BoxPanel(Orientation.Horizontal)
      addUndoRedoButton(p)
      contents += p

      control.publishUndoRedoStatus()
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Button() {
          val b : Button = this
          action = new Action("Save refactoring plan") {
            def apply(): Unit = {
              saveFile(control.filesHandler.workingDirectory, b.peer) match {
                case None => publisher publish Log("no file selected")
                case Some(f) =>  publisher publish SaveRecord(f)
              }
            }
          }
        }

        contents += new Button() {
          val b : Button = this
          action = new Action("Save refactoring plan") {
            def apply(): Unit = {
              openFile(control.filesHandler.workingDirectory, b.peer) match {
                case None => publisher publish Log("no file selected")
                case Some(f) => publisher publish LoadRecord(f)
              }
            }
          }
        }
      }.leftGlued

      contents += makeButton("Switch UML/Tree view", ""){
        () => publisher publish SwitchView
      }

      contents += makeButton("Show recording", ""){
        control.printRecording
      }

      contents += makeButton("Focus on Violations",
        "Display a visual representation of the graph"){
        () => publisher publish
          VisibilityEvent(VisibilitySet.violationsOnly(control.graph))
      }

      val testCommutativityCB = new CheckBox("Test commutativity")

      contents += testCommutativityCB

      contents += makeButton("Generate Code",
        "Apply transformations on the code")(
        () =>publisher publish
          GenCode(compareOutput = testCommutativityCB.selected))
    }


  reactions += {
    case GraphUpdate(g) =>
      contents.clear()
      addAlwaysVisibleButtons()
      addLoadedGraphButtons()
      revalidate()
  }


}
