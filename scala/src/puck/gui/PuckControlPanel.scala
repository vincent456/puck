package puck.gui

import puck.javaAG.nodeKind.JavaNodeKind
import puck.javaAG.{JavaNode, JavaDefaultDecisionMaker}

import scala.concurrent.Future

import scala.swing._
import puck.FilesHandler
import java.io.{OutputStream, PipedInputStream, PipedOutputStream, File}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import AST.LoadingListener
import scala.util.{Failure, Success}

/**
 * Created by lorilan on 08/05/14.
 */

class PuckControlPanel(val filesHandler : FilesHandler, val out :OutputStream)
  extends SplitPane(Orientation.Vertical){

  val leftWidth = PuckMainPanel.width * 1/3
  val rightWidth = PuckMainPanel.width *2/3
  val height = PuckMainPanel.height * 2/3

  val treeDisplayer = new GraphExplorer[JavaNodeKind](rightWidth/2, height)
  treeDisplayer.listenTo(this)


  val nodeInfos = new ScrollPane(){
    minimumSize = new Dimension(rightWidth/2, height)
    preferredSize = minimumSize

    reactions += {
      case PuckTreeNodeClicked(n) =>
        contents = new NodeInfosPanel[JavaNodeKind](filesHandler, n.agNode.asInstanceOf[JavaNode])

    }
  }
  nodeInfos.listenTo(treeDisplayer)

  val delayedDisplay = ArrayBuffer[Component]()

  def leftGlued(c : Component) : Component = {
    new BoxPanel(Orientation.Horizontal) {
      contents += c
      contents += Swing.HGlue
    }
  }

  def makeButton(title:String, tip: String)(act:() => Unit): Component =
    leftGlued(new Button() {
      tooltip = tip
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize

      action = new Action(title) {
        def apply() {
          Console.withOut(out) {
            Console.withErr(out) {
              act()
            }
          }
        }
      }
    })

  val progressBar  = new ProgressBar()

  def loadCode( onSuccess : => Unit = println("Graph loaded") ) = Future {
    progressBar.visible = true
    progressBar.value = 0

    val ag = filesHandler.loadGraph(new LoadingListener {
      override def update(loading: Double): Unit =
        progressBar.value = (loading * 100).toInt
    })
    progressBar.visible = false
    publish(AccessGraphModified(ag))

    delayedDisplay.foreach(_.visible = true)
  } onComplete {
    case Success(_) => onSuccess
    case Failure(exc) =>
      progressBar.visible = false
      exc.printStackTrace()
  }

  def loadConstraints() =
    try {
      print("Loading constraints ...")
      filesHandler.graph.discardConstraints()
      filesHandler.parseConstraints()
      println(" done:")
      filesHandler.graph.printConstraints()
    }
    catch{
      case e : Error => println("\n" + e.getMessage)
    }

  leftComponent = new BoxPanel(Orientation.Vertical) {
    minimumSize = new Dimension(leftWidth, height)

    contents += makeButton("Settings", "To set graphviz dot path"){
      () => val frame = new SettingsFrame(filesHandler)
        frame.visible = true
    }

    contents += makeButton("Sources",
      "Select the root directory containing the java (up to 1.5) source code you want to analyse"){
      () => val fc = new FileChooser(filesHandler.srcDirectory)
        fc.title = "What directory contains your Java application ?"
        fc.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
        fc showDialog(null, "Select")
        var f: File = fc.selectedFile
        if (f != null) {
          f = f.getCanonicalFile
          if (!(f == filesHandler.srcDirectory)) {
            filesHandler.srcDirectory = f
            loadCode(loadConstraints())

          }
        }
        println("Application directory : ")
        println(filesHandler.srcDirectory)
    }

    contents += makeButton("Decouple",
      "Select the file containing the decoupling constraints"){
      () => val fc = new FileChooser(filesHandler.srcDirectory)
        fc.title = "What directory contains your Java application ?"
        fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
        fc showDialog(null, "Select")
        var f: File = fc.selectedFile
        if (f != null) {
          f = f.getCanonicalFile
          if (!(f == filesHandler.decouple)) {
            filesHandler.decouple = f
            if(filesHandler.graph != null)
              loadConstraints()
            //filesHandler.getPrologHandler.setGenDir(f)
          }
        }
        println("Decouple : ")
        println(filesHandler.decouple)
    }

    contents += makeButton("Jar list file",
      "Select a file containing a list of the jar libraries required by the analysed program"){
      () =>  val fc = new FileChooser(filesHandler.srcDirectory)
        fc.title = "Select jar list file"
        fc.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
        fc showDialog(null, "Select")
        val f = fc.selectedFile
        if(f!=null){
          filesHandler.jarListFile = f
        }
    }


    progressBar.min = 0
    progressBar.max = 100
    progressBar.value = 0
    progressBar.labelPainted = true
    progressBar.visible = false

    contents += makeButton("(Re)load code + cts",
      "Load the selected source code and build the access graph"){
      () =>
        loadCode(loadConstraints())
    }
    contents += progressBar

    //delayedDisplay+= graphLoader.progressBar

    //val run = makeButton("Eval constraint",
    // "Launch the coupling constraint evaluation")

    val loadConstraintsButton = makeButton("(Re)load constraints",
      "Decorate the graph with the constraints of the selected decouple file"){ loadConstraints }

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
      () =>
        filesHandler.graph.printConstraints()
    }

    addDelayedComponent(showConstraints)

    val show = makeButton("Show graph",
      "Display a visual representation of the graph"){
      () =>
        Future {
          print("Printing graph ...")

          val pipedOutput = new PipedOutputStream()
          val pipedInput = new PipedInputStream(pipedOutput)

          Future {
            val imgframe = ImageFrame(pipedInput)
            imgframe.visible = true
          }

          if(filesHandler.makePng(soutput = Some(pipedOutput)) ==0)
            println("success")
          else
            println("fail")


        }

    }
    addDelayedComponent(show)


    val decisionStrategy = new CheckBox("GUI Decision Maker")
    addDelayedComponent(leftGlued(decisionStrategy))
    val printTrace = new CheckBox("Print trace")
    addDelayedComponent(leftGlued(printTrace))

    val solve = makeButton("Solve", "solve the loaded constraints"){
      () =>
        val f = Future {
          println("Solving constraints ...")
          filesHandler.solve(
            decisionMaker = if(decisionStrategy.selected)
              new GUIDecisionMaker(filesHandler.graph)
            else
              new JavaDefaultDecisionMaker(filesHandler.graph),
            trace = printTrace.selected

          )
        }

        f onComplete{
          case Success(_) =>
            PuckControlPanel.this.publish(AccessGraphModified(filesHandler.graph))
            println("Solving done")
          case Failure(exc) => println(exc.printStackTrace())
        }
    }

    addDelayedComponent(solve)

    val explore = makeButton("Explore", "search all solutions (gui decision maker unused)"){
      () =>
        val f = Future {
          println("Solving constraints ...")
          filesHandler.explore(trace = printTrace.selected)
        }

        f onComplete{
          case Success(_) => println("Solving done")
          case Failure(exc) => println(exc.printStackTrace())
        }
    }

    addDelayedComponent(explore)


  }

  rightComponent = new SplitPane(Orientation.Vertical) {
    leftComponent = treeDisplayer
    rightComponent = nodeInfos
  }


}
