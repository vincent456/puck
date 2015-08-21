package puck.gui

import java.awt.Dimension
import java.io.File

import puck.graph.{TypeDecl, NameSpace, GraphUtils}
import puck.graph.io.{VisibilitySet, FilesHandler}
import puck.gui.explorer.{SetVisibleFromKind, NodeInfosPanel, PuckTreeNodeClicked, GraphExplorer}
import puck.gui.search.ResultPanel
import puck.util.{PuckLogger, PuckLog}


import scala.collection.mutable.ArrayBuffer
import scala.swing._

class PuckInterfacePanel
( logger : PuckLogger,
  filesHandler : FilesHandler,
  graphUtils: GraphUtils
  ) extends SplitPane(Orientation.Vertical) {

  val leftWidth = PuckMainPanel.width * 3/8
  val rightWidth = PuckMainPanel.width * 5/8
  val height = PuckMainPanel.height * 2/3

  val treeDisplayer = new GraphExplorer(rightWidth/2, height)

  val treeDisplayerWrapper = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize
    contents = treeDisplayer
  }

  val progressBar  = new ProgressBar()
  val delayedDisplay = ArrayBuffer[Component]()
  val control = new PuckControl(logger, filesHandler, graphUtils, progressBar, delayedDisplay)

  val printIdsBox = new CheckBox("Show nodes ID")
  val printSignaturesBox = new CheckBox("Show signagures")

  def printIds() = printIdsBox.selected
  def printSigs() = printSignaturesBox.selected

  val nodeInfos = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize

    reactions += {
      case PuckTreeNodeClicked(graph, n) =>
        val nodeInfoPanel = new NodeInfosPanel(graph, n, printIds, printSigs, treeDisplayer.visibilitySet)
        contents = nodeInfoPanel
        control.listenTo(nodeInfoPanel)
        treeDisplayer.listenTo(nodeInfoPanel)

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
    treeDisplayer listenTo control

    reactions += {
      case ExplorationFinished(res0) =>
        resultsWrapper.contents.clear()
        val searchResultPanel =
          new ResultPanel(control.dg2AST.initialRecord, res0, logger,
            printIds, printSigs, treeDisplayer.visibilitySet)
        resultsWrapper.contents += searchResultPanel
        resultsWrapper.revalidate()
        control listenTo searchResultPanel

    }

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
        if( f != null && !(f == filesHandler.srcDirectory.get)) {
          filesHandler.setWorkingDirectory(f)
          publish(LoadCodeRequest())
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
      () => publish(LoadCodeRequest())
    }
    contents += progressBar

    val loadConstraintsButton = makeButton("(Re)load constraints",
      "Decorate the graph with the constraints of the selected decouple file"){
      () => publish(LoadConstraintRequest())
    }

    def addDelayedComponent(c : Component) : Unit = {
      c.visible = false
      contents += c
      delayedDisplay += c
      ()
    }

    addDelayedComponent(loadConstraintsButton)

    /*val printPl = makeButton("Print prolog",
      "Print a prolog version of the graph"){() => filesHandler.makeProlog()}

    addDelayedComponent(printPl)
*/
    val showConstraints = makeButton("Show constraints",
      "Show the constraints the graph has to satisfy"){
      () => publish(ConstraintDisplayRequest(control.dg2AST.initialGraph))
    }

    addDelayedComponent(showConstraints)

    import graphUtils.nodeKindKnowledge.kindOfKindType
    addDelayedComponent(new BoxPanel(Orientation.Horizontal){
      contents += new BoxPanel(Orientation.Vertical){
        contents+= new Button() {
          tooltip = "Make packages only visible"
          action = new Action("Package Visibility") {
            def apply() : Unit = { control.publish(SetVisibleFromKind(kindOfKindType(NameSpace))) }
          }
        }

        contents+= new Button() {
          tooltip = "Make only packages classes visible"
          action = new Action("Package+Class Visibility") {
            def apply() : Unit = { control.publish(SetVisibleFromKind(kindOfKindType(TypeDecl))) }
          }
        }
      }


      contents+= new BoxPanel(Orientation.Vertical){
        contents += printIdsBox
        contents += printSignaturesBox
      }
    })



    val show = makeButton("Show graph",
      "Display a visual representation of the graph"){
      () => publish(GraphDisplayRequest(
        "Graph",
        control.dg2AST.initialGraph,
        printIdsBox.selected,
        printSignaturesBox.selected,
        treeDisplayer.visibilitySet))
    }

    addDelayedComponent(show)

    val showViolations = makeButton("Focus on violations",
      "Display a visual representation of the graph"){
      () => publish(GraphDisplayRequest(
        "Graph",
        control.dg2AST.initialGraph,
        printIdsBox.selected,
        printSignaturesBox.selected,
        VisibilitySet.violationsOnly(control.dg2AST.initialGraph)))
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
    leftComponent = treeDisplayerWrapper
    rightComponent = nodeInfos
  }
}
