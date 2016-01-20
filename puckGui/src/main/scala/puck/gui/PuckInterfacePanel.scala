package puck.gui

import java.awt.Dimension
import java.io.File

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


  val progressBar  = new ProgressBar()
  val control = new PuckControl(logger, filesHandler,
    graphUtils, progressBar)

  val graphExplorer = new GraphExplorer(treeIcons, graphUtils, control.printingOptionsControl)

  control listenTo this
  this listenTo control

  graphExplorer listenTo control
  control listenTo graphExplorer

  val nodeInfos = new ScrollPane(){
    //preferredSize = new Dimension(rightWidth/2, height)

    reactions += {
      case NodeClicked(n) if n.id != DependencyGraph.rootId =>
        Swing.onEDT {
          contents =
            new NodeInfosPanel(PuckInterfacePanel.this, control.graph, n.id,
              edge => new EdgeMenu(PuckInterfacePanel.this, edge,
                  control.printingOptionsControl,
                  control.graphStack.graph,
                  graphUtils)
            )
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
        () => PuckInterfacePanel.this publish LoadConstraintRequest
      }
      contents += makeButton("Show constraints",
        "Show the constraints the graph has to satisfy"){
        () => PuckInterfacePanel.this publish ConstraintDisplayRequest(g)
      }

      val p = new BoxPanel(Orientation.Horizontal)
      val handler = PuckEvents.addUndoRedoButton(p.peer, PuckInterfacePanel.this)
      reactions += {
        case urs : UndoRedoStatus => handler(urs)
      }
      this listenTo control
      contents += p

      control.publishUndoRedoStatus()
      contents += new BoxPanel(Orientation.Horizontal) {
        PuckEvents.addLoadSaveButton(this.peer, PuckInterfacePanel.this,
          control.filesHandler.workingDirectory)
      }.leftGlued

      contents += makeButton("Show DG in UML like view",
        "Display a visual representation of the graph"){
        () =>
          PuckInterfacePanel.this publish
            GraphDisplayRequest("Graph", g,
            VisibilitySet.topLevelVisible(g).
              hideWithName(g, Seq("@primitive")).
              hideWithName(g, Seq("java")))
      }

      contents += makeButton("Show DG in UML like view (Focus on Violations)",
        "Display a visual representation of the graph"){
        () => PuckInterfacePanel.this publish
          GraphDisplayRequest("Graph", g,
          VisibilitySet.violationsOnly(g))
      }

      contents += makeButton("Generate Code",
        "Apply transformations on the code")(
        () =>PuckInterfacePanel.this publish
          GenCode(compareOutput = false))
    }

  val loadedGraphButtonsWrapper = new FlowPanel()

  reactions += {
    case GraphUpdate(g) =>
      loadedGraphButtonsWrapper.contents.clear()
      loadedGraphButtonsWrapper.contents += loadedGraphButtons(g)
      PuckInterfacePanel.this.revalidate()
  }

  leftComponent = new BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension(leftWidth, height)

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
    resizeWeight = 0.25
    leftComponent = new BoxPanel(Orientation.Vertical) {
      contents += new Label("DG Explorer")
      contents += graphExplorer
    }
    rightComponent = nodeInfos
  }
}
