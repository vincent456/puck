package puck.gui

import java.awt.Dimension
import java.io.File

import puck.graph.DependencyGraph
import puck.graph.io.{FilesHandler, Hidden, VisibilitySet}
import puck.gui.explorer.{PackageOnlyVisible, NodeInfosPanel, PuckTreeNodeClicked, GraphExplorer}
import puck.gui.search.ResultPanel
import puck.util.PuckLog

import scala.collection.mutable.ArrayBuffer
import scala.swing._

/**
 * Created by lorilan on 22/02/15.
 */
class PuckInterfacePanel (filesHandler : FilesHandler) extends SplitPane(Orientation.Vertical) {

  val leftWidth = PuckMainPanel.width * 3/8
  val rightWidth = PuckMainPanel.width * 5/8
  val height = PuckMainPanel.height * 2/3

  val visibilitySet = VisibilitySet()
  visibilitySet.setVisibility(DependencyGraph.rootId /*:: (Predefined.list map (_.id))*/, Hidden)
  val treeDisplayer = new GraphExplorer(visibilitySet, rightWidth/2, height)

  val treeDisplayerWrapper = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize
    contents = treeDisplayer
  }

  val progressBar  = new ProgressBar()
  val delayedDisplay = ArrayBuffer[Component]()
  val control = new PuckControl(filesHandler, progressBar, delayedDisplay)

  val printIdsBox = new CheckBox("Show nodes ID")
  val printSignaturesBox = new CheckBox("Show signagures")

  def printIds() = printIdsBox.selected
  def printSigs() = printSignaturesBox.selected

  val nodeInfos = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize

    reactions += {
      case PuckTreeNodeClicked(graph, n) =>
        val nodeInfoPanel = new NodeInfosPanel(graph, n, printIds, printSigs, visibilitySet)
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
        def apply() { act() }
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
          new ResultPanel(filesHandler.initialRecord, res0, filesHandler.logger,
            printIds, printSigs, visibilitySet)
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
      () => val fc = new FileChooser(filesHandler.srcDirectory.get)
        fc.title = "What directory contains your application ?"
        fc.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        fc showDialog(null, "Select")
        val f: File = fc.selectedFile
        if( f != null && !(f == filesHandler.srcDirectory.get)) {
          filesHandler.setWorkingDirectory(f)
          publish(LoadCodeRequest())
        }
        filesHandler.logger.writeln("Application directory : ")
        filesHandler.logger.writeln(filesHandler.srcDirectory.toString)
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

    def addDelayedComponent(c : Component){
      c.visible = false
      contents += c
      delayedDisplay += c
    }

    addDelayedComponent(loadConstraintsButton)

    /*val printPl = makeButton("Print prolog",
      "Print a prolog version of the graph"){() => filesHandler.makeProlog()}

    addDelayedComponent(printPl)
*/
    val showConstraints = makeButton("Show constraints",
      "Show the constraints the graph has to satisfy"){
      () => publish(ConstraintDisplayRequest(filesHandler.graph))
    }

    addDelayedComponent(showConstraints)

    addDelayedComponent(new BoxPanel(Orientation.Horizontal){
      contents+= new Button() {
        tooltip = "Make packages only visible"
        action = new Action("Package Visibility") {
          def apply() { control.publish(PackageOnlyVisible()) }
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
        filesHandler.graph,
        printIdsBox.selected,
        printSignaturesBox.selected,
        visibilitySet))
    }

    addDelayedComponent(show)

    val showViolations = makeButton("Focus on violations",
      "Display a visual representation of the graph"){
      () => publish(GraphDisplayRequest(
        "Graph",
        filesHandler.graph,
        printIdsBox.selected,
        printSignaturesBox.selected,
        VisibilitySet.violationsOnly(filesHandler.graph)))
    }

    addDelayedComponent(showViolations)


    val searchStrategies = new ComboBox(filesHandler.searchingStrategies){
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize
    }

    addDelayedComponent(searchStrategies)

    val explore = makeButton("Solve with search engine",""){
      () => publish(ExploreRequest(searchStrategies.selection.item))
    }

    addDelayedComponent(explore)

    addDelayedComponent(resultsWrapper)

  }

  rightComponent = new SplitPane(Orientation.Vertical) {
    leftComponent = treeDisplayerWrapper
    rightComponent = nodeInfos
  }
}
