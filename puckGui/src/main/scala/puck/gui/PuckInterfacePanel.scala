package puck.gui

import java.awt.Dimension
import java.io.File
import javax.swing.{BoxLayout, JPanel}

import puck.FilesHandlerDG2ASTControllerOps
import puck.graph._
import puck.graph.io.{VisibilitySet, FilesHandler}
import puck.gui.explorer.{DGTreeIcons, NodeInfosPanel, GraphExplorer}
import puck.util.{PuckLogger, PuckLog}


import scala.swing._
import VisibilitySet.VisibilitySetOps

class PuckInterfacePanel
( logger : PuckLogger,
  filesHandler : FilesHandler,
  graphUtils: GraphUtils,
  treeIcons : DGTreeIcons
  ) extends SplitPane(Orientation.Vertical) {

  val leftWidth = PuckMainPanel.width * 3/8
  val rightWidth = PuckMainPanel.width * 5/8
  val height = PuckMainPanel.height * 2/3

  val graphExplorer = new GraphExplorer(treeIcons, graphUtils)



  val progressBar  = new ProgressBar()
  val control = new PuckControl(logger, filesHandler,
    graphUtils, progressBar) with FilesHandlerDG2ASTControllerOps

  control listenTo this
  this listenTo control

  graphExplorer listenTo control
  control listenTo graphExplorer

  val nodeInfos = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize

    reactions += {
      case NodeClicked(n) if n.id != DependencyGraph.rootId =>
        Swing.onEDT {
          val nodeInfoPanel = new NodeInfosPanel(this, control.graph, n.id)
          contents = nodeInfoPanel
          control listenTo nodeInfoPanel
          graphExplorer listenTo nodeInfoPanel
        }

    }
  }
  nodeInfos listenTo graphExplorer

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


  def loadedGraphButtons(g : DependencyGraph): BoxPanel =
    new BoxPanel(Orientation.Vertical){

      contents += makeButton("(Re)load constraints",
        "Decorate the graph with the constraints of the selected decouple file"){
        () => PuckInterfacePanel.this.publish(LoadConstraintRequest)
      }
      contents += makeButton("Show constraints",
        "Show the constraints the graph has to satisfy"){
        () => PuckInterfacePanel.this.
          publish(ConstraintDisplayRequest(g))
      }

      val p = new BoxPanel(Orientation.Horizontal)
      val handler = PuckEvents.addUndoRedoButton(p.peer, PuckInterfacePanel.this)
      reactions += {
        case urs : UndoRedoStatus => handler(urs)
      }
      this listenTo control
      contents += p

      control.publishUndoRedoStatus()


      PuckEvents.addVisibilityCheckBoxes(this.peer,
        PuckInterfacePanel.this,
        control.printingOptionsControl.printingOptions)


      contents += makeButton("Show graph",
        "Display a visual representation of the graph"){
        () =>
          publish(GraphDisplayRequest("Graph", g,
            VisibilitySet.topLevelVisible(g).
              hideWithName(g, Seq("@primitive")).
              hideWithName(g, Seq("java"))))
      }

      contents += makeButton("Show graph (Focus on violations)",
        "Display a visual representation of the graph"){
        () => publish(GraphDisplayRequest("Graph", g,
          VisibilitySet.violationsOnly(g)))
      }

      contents += makeButton("Generate Code",
        "Apply transformations on the code")(
        control.deleteOutDirAndapplyOnCode)
    }

  val loadedGraphButtonsWrapper = new FlowPanel()

  reactions += {
    case GraphUpdate(g) =>
      loadedGraphButtonsWrapper.contents.clear()
      loadedGraphButtonsWrapper.contents += loadedGraphButtons(g)
      leftComponent.revalidate()
  }

  leftComponent = new BoxPanel(Orientation.Vertical) {
    minimumSize = new Dimension(leftWidth, height)

    val resultsWrapper = new FlowPanel()

    control listenTo this
    this listenTo control


    contents += makeButton("Settings", "To set graphviz dot path"){
      () => val frame = new SettingsFrame(filesHandler)
        frame.visible = true
    }

    import PuckLog.defaultVerbosity

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
        logger.writeln("Application directory : ")
        logger.writeln(filesHandler.srcDirectory.toString)
    }

    progressBar.min = 0
    progressBar.max = 100
    progressBar.value = 0
    progressBar.labelPainted = true
    progressBar.visible = false


    contents += makeButton("(Re)load code & constraints",
      "Load the selected source code and build the access graph"){
      () => publish(LoadCodeRequest)
    }
    contents += progressBar

    contents += loadedGraphButtonsWrapper

  }

  rightComponent = new SplitPane(Orientation.Vertical) {
    leftComponent = new BoxPanel(Orientation.Vertical) {
      contents += new Label("Dependency Graph")
      contents += graphExplorer
    }
    rightComponent = nodeInfos
  }
}
