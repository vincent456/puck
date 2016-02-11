package puck.gui

import java.awt.Dimension
import java.io.File

import puck.graph.io.Project.Default
import puck.graph.io.{ConfigParser, Project, VisibilitySet}
import scala.swing._
import scala.swing.SequentialContainer.Wrapper

class PuckInterfacePanel
( control : PuckControl
) extends BoxPanel(Orientation.Vertical)  {

  private val publisher = control.Bus
  this listenTo control.Bus

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

  import control.project

  def addAlwaysVisibleButtons(): Unit ={
    contents += makeButton("Settings", "To set graphviz dot path"){
      () => val frame = new SettingsFrame(project)
        frame.visible = true
    }

    contents += makeButton("Load project",
      "Select a workspace"){
      () =>
        val fc = new FileChooser(project.workspace){
          title = "What directory contains your application ?"
          fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        }

        fc showDialog(null, "Select")
        val f: File = fc.selectedFile
        if( f != null && !(f == project.workspace)) {
          control.loadConf(f)
          publisher publish LoadCodeRequest
        }
        val sf : Option[File]= project.someFile(Project.Keys.workspace)
        val path = sf map (_.getAbsolutePath) getOrElse "No directory selected"
        publisher publish Log(s"Workspace directory :\n$path")
    }

    contents += makeButton("(Re)load code & constraints",
      "Load the selected source code and build the access graph"){
      () => publisher publish LoadCodeRequest
    }
    contents += control.progressBar
  }

  addAlwaysVisibleButtons()


  def addUndoRedoButton(c : Wrapper) : Unit = {
    import control.graphStack
    c.contents +=
      new Button(new Action("Undo all") {

        enabled = false

        def apply() = graphStack.undoAll()

        reactions += {
          case _ : GraphStackEvent =>
            enabled = graphStack.canUndo
        }
        listenTo(control.Bus)
      })

    c.contents +=
      new Button(new Action("Undo") {

        enabled = false

        def apply() = graphStack.undo()

        reactions += {
          case _ : GraphStackEvent =>
            enabled = graphStack.canUndo
        }
        listenTo(control.Bus)
      })

    c.contents +=
      new Button(new Action("Redo") {

        enabled = false

        def apply() = graphStack.redo()

        reactions += {
          case _ : GraphStackEvent =>
            enabled = graphStack.canRedo
        }
        listenTo(control.Bus)
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

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Button() {
          val b : Button = this
          action = new Action("Save refactoring plan") {

            def apply(): Unit = {
              saveFile(control.project.workspace, b.peer) match {
                case None => publisher publish Log("no file selected")
                case Some(f) =>  publisher publish SaveRecord(f)
              }
            }
          }
        }

        contents += new Button() {
          val b : Button = this
          action = new Action("Load refactoring plan") {
            def apply(): Unit = {
              openFile(control.project.workspace, b.peer) match {
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
          VisibilityEvent(control.graph, VisibilitySet.violationsOnly(control.graph))
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
