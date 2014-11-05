package puck.gui

import puck.graph.{NodeId, AccessGraph, FilesHandler, NodeKind, ResultT}
import puck.search.Search
import puck.util.{PuckLog, PuckLogger}

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import java.awt.Dimension
import java.io.File


/**
 * Created by lorilan on 08/05/14.
 */

object PuckMainPanel{
  val width = 1024
  val height = 800

  def leftGlued(c : Component) : BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += c
      contents += Swing.HGlue
    }
  }
}

class PuckMainPanel[Kind <: NodeKind[Kind], T](val filesHandler: FilesHandler[Kind, T])
  extends SplitPane(Orientation.Horizontal){
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val console = new TextArea()
  console.editable = false

  class ConsoleLogger(val askPrint : PuckLog.Verbosity => Boolean) extends PuckLogger{

    def writeln(msg : => Any)(implicit v : PuckLog.Verbosity){
      if(mustPrint(v)) {
        console.append(preMsg(v) + msg)
        console.append(System.lineSeparator())
      }
    }
    def write(msg : => Any)(implicit v : PuckLog.Verbosity){
      if(mustPrint(v)) {
        console.append(preMsg(v) + msg)
      }
    }

  }

  filesHandler.logger = new ConsoleLogger(filesHandler.logPolicy)

  //topComponent
  leftComponent = new SplitPane(Orientation.Vertical) {
    val leftWidth = PuckMainPanel.width * 1/3
    val rightWidth = PuckMainPanel.width *2/3
    val height = PuckMainPanel.height * 2/3

    val treeDisplayer = new GraphExplorer[Kind, T](rightWidth/2, height)

    val progressBar  = new ProgressBar()
    val delayedDisplay = ArrayBuffer[Component]()
    val control = new PuckControl(filesHandler, progressBar, delayedDisplay)

    val nodeInfos = new ScrollPane(){
      minimumSize = new Dimension(rightWidth/2, height)
      preferredSize = minimumSize

      reactions += {
        case PuckTreeNodeClicked(graph, n) =>
          val nodeInfoPanel = new NodeInfosPanel(graph.asInstanceOf[AccessGraph[Kind, T]], n.asInstanceOf[NodeId[Kind]])
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
          val searchResultPanel = new SearchResultPanel[Kind, T](filesHandler.initialRecord,
            res0.asInstanceOf[Search[ResultT[Kind, T]]],
            filesHandler.logger)
          resultsWrapper.contents += searchResultPanel
          control listenTo searchResultPanel

      }

      val decisionStrategy = new CheckBox("GUI Decision Maker")
      val printTrace = new CheckBox("Print trace")


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
        () => filesHandler.graph.printConstraints(filesHandler.logger, defaultVerbosity)
      }

      addDelayedComponent(showConstraints)

      val show = makeButton("Show graph",
        "Display a visual representation of the graph"){
        () => publish(GraphDisplayRequest("Graph", filesHandler.graph))
      }

      addDelayedComponent(show)

      val searchStrategies = new ComboBox(filesHandler.searchingStrategies){
        minimumSize = new Dimension(leftWidth, 30)
        maximumSize = minimumSize
        preferredSize = minimumSize
      }

      addDelayedComponent(searchStrategies)

      val explore = makeButton("Solve with search engine",""){
        () => publish(ExploreRequest(printTrace.selected,
          searchStrategies.selection.item))
      }

      addDelayedComponent(explore)

      addDelayedComponent(resultsWrapper)

    }

    rightComponent = new SplitPane(Orientation.Vertical) {
      leftComponent = treeDisplayer
      rightComponent = nodeInfos
    }

  }
  /*console.preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height * 1 / 3)
  console.minimumSize = console.preferredSize*/

  //bottomComponent
  rightComponent = new BoxPanel(Orientation.Vertical) {
    contents += new ScrollPane(console)

    contents += new Button(){
      tooltip = "Clear the console"
      /*minimumSize =
      maximumSize = minimumSize
      preferredSize = minimumSize*/

      action = new Action("Clear"){ def apply(){
        console.text = ""
      }
      }
    }
  }
}

