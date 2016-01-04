package puck.gui

import java.awt.Dimension
import java.io.File

import puck.graph._
import puck.graph.io.{VisibilitySet, FilesHandler}
import puck.gui.explorer.{NodeInfosPanel, GraphExplorer}
import puck.util.{PuckLogger, PuckLog}


import scala.collection.mutable.ArrayBuffer
import scala.swing._
import VisibilitySet.VisibilitySetOps

class PuckInterfacePanel
( logger : PuckLogger,
  filesHandler : FilesHandler,
  graphUtils: GraphUtils
  ) extends SplitPane(Orientation.Vertical) {

  val leftWidth = PuckMainPanel.width * 3/8
  val rightWidth = PuckMainPanel.width * 5/8
  val height = PuckMainPanel.height * 2/3

  val treeDisplayer = new GraphExplorer()



  val progressBar  = new ProgressBar()
  val delayedDisplay = ArrayBuffer[Component]()
  val control = new PuckControl(logger, filesHandler,
    graphUtils, progressBar, delayedDisplay)

  control registerAsStackListeners treeDisplayer

  reactions += {
    case GraphExplorerFocus(e) =>
      treeDisplayer.filter = Some(e)
      treeDisplayer.update(control)
  }

  val printIdsBox = new CheckBox("Show nodes ID")
  val printSignaturesBox = new CheckBox("Show signagures")

  def printIds() = printIdsBox.selected
  def printSigs() = printSignaturesBox.selected

  val nodeInfos = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize

    reactions += {
      case NodeClicked(n) if n.id != DependencyGraph.rootId =>
        Swing.onEDT {
          val nodeInfoPanel = new NodeInfosPanel(control.graph, n.id) {
            def onEdgeButtonClick(source: NodeId, target: NodeId): Unit = {
              this publish
                GraphDisplayRequest("Graph with uses selected",
                  graph, printIds(), printSigs(),
                  VisibilitySet.topLevelVisible(g).
                    hideWithName(g, Seq("@primitive")).
                    hideWithName(g, Seq("java")),
                  sUse = Some(Uses(source, target)))
            }
          }
          contents = nodeInfoPanel
          control.listenTo(nodeInfoPanel)
          treeDisplayer.listenTo(nodeInfoPanel)
        }

    }
  }
  nodeInfos listenTo treeDisplayer

  def makeButton(title:String, tip: String)(act:() => Unit): Component =
    PuckMainPanel.leftGlued(new Button() {
      tooltip = tip
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize

      action = new Action(title) {
        def apply() : Unit = { act() }
      }
    })

  leftComponent = new BoxPanel(Orientation.Vertical) {
    minimumSize = new Dimension(leftWidth, height)

    val resultsWrapper = new FlowPanel()

    control listenTo this
    this listenTo control


//    reactions += {
//      case ExplorationFinished(res0) =>
//        resultsWrapper.contents.clear()
//        val searchResultPanel =
//          new ResultPanel(control.dg2AST.initialRecord, res0, logger,
//            printIds, printSigs, treeDisplayer.visibilitySet)
//        resultsWrapper.contents += searchResultPanel
//        resultsWrapper.revalidate()
//        control listenTo searchResultPanel
//
//    }

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

    val loadConstraintsButton = makeButton("(Re)load constraints",
      "Decorate the graph with the constraints of the selected decouple file"){
      () => publish(LoadConstraintRequest)
    }

    def addDelayedComponent(c : Component) : Unit = {
      c.visible = false
      contents += c
      delayedDisplay += c
      ()
    }

    addDelayedComponent(loadConstraintsButton)

    val showConstraints = makeButton("Show constraints",
      "Show the constraints the graph has to satisfy"){
      () => publish(ConstraintDisplayRequest(control.dg2ast.initialGraph))
    }

    addDelayedComponent(showConstraints)

    val show = makeButton("Show graph",
      "Display a visual representation of the graph"){
      () =>
        val g = control.dg2ast.initialGraph
        publish(GraphDisplayRequest(
          "Graph",
          g, printIdsBox.selected,
             printSignaturesBox.selected,
          VisibilitySet.topLevelVisible(g).
            hideWithName(g, Seq("@primitive")).
            hideWithName(g, Seq("java"))))
    }

    addDelayedComponent(show)

    val showViolations = makeButton("Show graph (Focus on violations)",
      "Display a visual representation of the graph"){
      () => publish(GraphDisplayRequest(
        "Graph",
        control.dg2ast.initialGraph,
        printIdsBox.selected,
        printSignaturesBox.selected,
        VisibilitySet.violationsOnly(control.dg2ast.initialGraph)))
    }

    addDelayedComponent(showViolations)




//    val searchStrategies = new ComboBox(filesHandler.searchingStrategies){
//      minimumSize = new Dimension(leftWidth, 30)
//      maximumSize = minimumSize
//      preferredSize = minimumSize
//    }
//
//    addDelayedComponent(searchStrategies)
//
//    val explore = makeButton("Solve with search engine",""){
//      () => publish(ExploreRequest(searchStrategies.selection.item))
//    }
//
//    addDelayedComponent(explore)
//
//    addDelayedComponent(resultsWrapper)

  }

  rightComponent = new SplitPane(Orientation.Vertical) {
    leftComponent = new BoxPanel(Orientation.Vertical) {
      contents += new Label("Dependency Graph")
      contents += treeDisplayer
    }
    rightComponent = nodeInfos
  }
}
