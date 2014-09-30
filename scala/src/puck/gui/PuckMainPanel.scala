package puck.gui

import puck.graph.NodeKind
import puck.graph.backTrack.Recording
import puck.graph.backTrack.comparison.RecordingComparator
import puck.graph.constraints.search.ConstraintSolving
import puck.graph.io.FilesHandler
import puck.util.IntLogger

import scala.collection.mutable.ArrayBuffer
import scala.swing._
import java.awt.Dimension
import java.io.File


/**
 * Created by lorilan on 08/05/14.
 */

object PuckMainPanel{
  val width = 600
  val height = 800

  def leftGlued(c : Component) : BoxPanel = {
    new BoxPanel(Orientation.Horizontal) {
      contents += c
      contents += Swing.HGlue
    }
  }

}

class PuckMainPanel[Kind <: NodeKind[Kind]](val filesHandler: FilesHandler[Kind])
  extends SplitPane(Orientation.Horizontal){
  dividerSize = 3

  preferredSize = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val console = new TextArea()
  console.editable = false

  class ConsoleLogger extends IntLogger{
    def writeln(msg : => String, v : Int){
      if(mustPrint(v)) {
        console.append(msg)
        console.append(System.lineSeparator())
      }
    }
    def write(msg : => String, v : Int){
      if(mustPrint(v)) {
        console.append(msg)
      }
    }

  }

  filesHandler.logger = new ConsoleLogger()

  /*val out = new OutputStream {
    override def write(p1: Int): Unit = console.append(String.valueOf(p1.toChar))
  }*/

  //topComponent
  leftComponent = new  SplitPane(Orientation.Vertical) {
    val leftWidth = PuckMainPanel.width * 1/3
    val rightWidth = PuckMainPanel.width *2/3
    val height = PuckMainPanel.height * 2/3

    val treeDisplayer = new GraphExplorer[Kind](rightWidth/2, height)

    val progressBar  = new ProgressBar()
    val delayedDisplay = ArrayBuffer[Component]()
    val control = new PuckControl(filesHandler, progressBar, delayedDisplay)

    val nodeInfos = new ScrollPane(){
      minimumSize = new Dimension(rightWidth/2, height)
      preferredSize = minimumSize

      reactions += {
        case PuckTreeNodeClicked(n) =>
          val nip = new NodeInfosPanel[Kind]( n.asInstanceOf[PuckTreeNode[Kind]].agNode)
          contents = nip
          control.listenTo(nip)
          treeDisplayer.listenTo(nip)

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


      val resultsWrapper = new BoxPanel(Orientation.Vertical)
      var resultsCB : CSSearchStateComboBox[Kind] = _

      control listenTo this
      this listenTo control
      treeDisplayer listenTo control

      reactions += {
        case ExplorationFinished(res0) =>
          resultsWrapper.contents.clear()

          def filterDifferentStates(l : List[ST]): List[ST] = {

            def aux(l : List[ST], acc : List[ST]) : List[ST] = {
              if (l.nonEmpty) {
                aux(l.tail,
                  if (!l.tail.exists { st => st.result.produceSameGraph(l.head.result)})
                    l.head :: acc
                  else acc)
              }
              else acc
            }
            aux(l, List())
          }

          type ST = ConstraintSolving.FinalState[Kind]
          val res = res0.asInstanceOf[List[ConstraintSolving.FinalState[Kind]]]
          filesHandler.logger.writeln("%d final states".format(res.size))

          /*filesHandler.logger.write("compute different final states (without sorting by coupling first) : ")

          val plop = puck.util.Time.time(filesHandler.logger){
            filterDifferentStates(res)
          }
          filesHandler.logger.writeln("%d final states".format(plop.size))
*/
          filesHandler.logger.write("compute different final states (after sorting by coupling value) : ")
          val sortedRes  =
            puck.util.Time.time(filesHandler.logger){

              //CSSearchStateComboBox.sort(res).mapValues(filterDifferentStates)
              // do not actually apply the function and hence give a false compute time
              CSSearchStateComboBox.sort(res).foldLeft(Map[Int, List[ConstraintSolving.FinalState[Kind]]]()){
                case (acc, (k, v)) => acc + (k -> filterDifferentStates(v))
              }
            }

          val total = sortedRes.foldLeft(0) { case (acc, (_, l)) => acc + l.size}

          filesHandler.logger.writeln("%d final states".format(res.size))
          filesHandler.logger.writeln("%d different final states ".format(total))

          resultsCB = new CSSearchStateComboBox(sortedRes)
          this listenTo resultsCB

          resultsWrapper.contents += resultsCB
          resultsWrapper.contents += new FlowPanel(){
            contents += new Label("Compare")
            val cb1 = new CSSearchStateComboBox(sortedRes)
            val cb2 = new CSSearchStateComboBox(sortedRes)
            contents += cb1
            contents += new Label("and")
            contents += cb2
            contents += Button(">>"){
              val recording1 = cb1.selectedState.result
              val recording2 = cb2.selectedState.result
              println("Comparing %s and %s".format(recording1, recording2))
              new RecordingComparator(recording1, recording2).search() match {
                case None => println("no mapping")
                case Some(st) => println(st.result)
              }
            }
          }

          resultsWrapper.contents += new FlowPanel(){
            val couplingValues = new ComboBox(sortedRes.keys.toSeq)
            contents += couplingValues
            contents += Button("Print"){
              val d = filesHandler.graphFile("_results")
              d.mkdir()
              val subDir = filesHandler.graphFile("_results%c%d".format(File.separatorChar,
                couplingValues.selection.item))
              subDir.mkdir()
              filesHandler.printCSSearchStatesGraph(subDir, sortedRes(couplingValues.selection.item))
            }
          }

          resultsWrapper.contents += Button("Print all"){
            filesHandler.printCSSearchStatesGraph(sortedRes)
          }

        case StateSelected(cb) if cb == resultsCB =>
          resultsCB.selectedState.result()

      }

      val decisionStrategy = new CheckBox("GUI Decision Maker")
      val printTrace = new CheckBox("Print trace")


      contents += makeButton("Settings", "To set graphviz dot path"){
        () => val frame = new SettingsFrame(filesHandler)
          frame.visible = true
      }

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


      contents += makeButton("Do it !",
        "Magic !"){
        () => publish(DoWholeProcessRequest(printTrace.selected))
      }

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

      val printPl = makeButton("Print prolog",
        "Print a prolog version of the graph"){() => filesHandler.makeProlog()}

      addDelayedComponent(printPl)

      val showConstraints = makeButton("Show constraints",
        "Show the constraints the graph has to satisfy"){
        () => filesHandler.graph.printConstraints(filesHandler.logger)
      }

      addDelayedComponent(showConstraints)

      val show = makeButton("Show graph",
        "Display a visual representation of the graph"){
        () => publish(GraphDisplayRequest())
      }

      addDelayedComponent(show)

      addDelayedComponent(PuckMainPanel.leftGlued(decisionStrategy))
      addDelayedComponent(PuckMainPanel.leftGlued(printTrace))

      val solve = makeButton("Solve", "solve the loaded constraints"){
        () => publish(SolveRequest(if(decisionStrategy.selected)
          new GUIDecisionMaker(filesHandler)
        else
          filesHandler.decisionMaker(),
          printTrace.selected))

      }

      addDelayedComponent(solve)

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

      val printCode = makeButton("Apply on code",
        "apply the planned modifications on the code"){
        () => publish(ApplyOnCodeRequest())
      }
      addDelayedComponent(printCode)
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

