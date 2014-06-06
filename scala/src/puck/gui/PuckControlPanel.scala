package puck.gui

import scala.concurrent.Future

import scala.swing._
import puck.FilesHandler
import java.io.{OutputStream, PipedInputStream, PipedOutputStream, File}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import AST.LoadingListener
import puck.javaAG.{DefaultDecisionMaker, JavaSolver}
import scala.util.{Failure, Success}
import puck.graph.constraints.DecisionMaker

/**
 * Created by lorilan on 08/05/14.
 */

class PuckControlPanel(val filesHandler : FilesHandler, val out :OutputStream)
  extends SplitPane(Orientation.Vertical){

  val leftWidth = PuckMainPanel.width * 1/3
  val rightWidth = PuckMainPanel.width *2/3
  val height = PuckMainPanel.height * 2/3

  val treeDisplayer = new ScrollPane(){
    minimumSize = new Dimension(rightWidth, height)
    preferredSize = minimumSize
  }
  val delayedDisplay = ArrayBuffer[Component]()

  def makeButton(title:String, tip: String)(act:() => Unit): Button =
    new Button(){
      tooltip = tip
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize

      action = new Action(title){ def apply(){
        Console.withOut(out) {
          Console.withErr(out) {
            act()
          }
        }
      }
      }
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
            //filesHandler.getPrologHandler.setGenDir(f)
            //decoupleEditor.setEditedFile(filesHandler.getPrologHandler.getDecouple)
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

    val progressBar  = new ProgressBar()
    progressBar.min = 0
    progressBar.max = 100
    progressBar.value = 0
    progressBar.labelPainted = true
    progressBar.visible = false

    contents += makeButton("Load code",
      "Load the selected source code and build the access graph"){
      () =>
        val f: Future[Unit] = Future {
          progressBar.visible = true
          progressBar.value = 0

          val ag = filesHandler.loadGraph(new LoadingListener {
            override def update(loading: Double): Unit =
              progressBar.value = (loading * 100).toInt
          })
          progressBar.visible = false
          val ppController = new PackagePanelController(ag)
          treeDisplayer.contents = Component.wrap(ppController.tree)
          delayedDisplay.foreach(_.visible = true)
        }
        f onComplete {
          case Success(_) => println("Graph loaded")
          case Failure(exc) =>
            progressBar.visible = false
            println(exc.getMessage)
        }
    }
    contents += progressBar

    //delayedDisplay+= graphLoader.progressBar

    //val run = makeButton("Eval constraint",
    // "Launch the coupling constraint evaluation")

    val loadConstraints = makeButton("Load constraints",
    "Decorate the graph with the constraints of the selected decouple file"){
      () =>
        print("Loading constraints ...")
        try {
          filesHandler.parseConstraints()
          println(" done:")
          filesHandler.accessGraph.printConstraints()
        }
        catch{
          case e : Error => println("\n" + e.getMessage)
        }

    }
    loadConstraints.visible = false
    contents += loadConstraints
    delayedDisplay += loadConstraints

    val showConstraints = makeButton("Show constraints",
      "Show the constraints the graph has to satisfy"){
      () =>
          filesHandler.accessGraph.printConstraints()
    }
    showConstraints.visible = false
    contents += showConstraints
    delayedDisplay += showConstraints

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
    show.visible = false
    contents +=show
    delayedDisplay += show

    val decisionStrategy = new ComboBox[DecisionMaker](List(GUIDecisionMaker,
    DefaultDecisionMaker)){
      minimumSize = new Dimension(leftWidth, 30)
      maximumSize = minimumSize
      preferredSize = minimumSize
    }

    decisionStrategy.visible = false
    contents += decisionStrategy
    delayedDisplay += decisionStrategy


    val solve = makeButton("Solve", "solve the loaded constraints"){
      () =>
        val f = Future {
          println("Solving constraints ...")
          filesHandler.solve(decisionMaker = decisionStrategy.selection.item)
        }

        f onComplete{
          case Success(_) => println("Solving done")
          case Failure(exc) => println(exc.getMessage)
        }
    }

    solve.visible = false
    contents += solve
    delayedDisplay += solve
  }

  rightComponent = treeDisplayer


}
