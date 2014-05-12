package puck.gui

import scala.concurrent.future
import scala.concurrent.Future

import scala.swing._
import puck.FilesHandler
import java.io.{PipedInputStream, PipedOutputStream, File}
import puck.graph.{AGNode, AccessGraph}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import AST.LoadingListener

/**
 * Created by lorilan on 08/05/14.
 */
class PuckControlPanel(val filesHandler : FilesHandler)
  extends SplitPane(Orientation.Vertical){

  val leftWidth = 100
  val rightWidth = 200
  val height = 300

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

      action = new Action(title){ def apply(){act()}}
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
            System.err.println("onAppDirectoryChoice : TO FINISH !!")
            //decoupleEditor.setEditedFile(filesHandler.getPrologHandler.getDecouple)
          }
        }
        println("Application directory : ")
        println(filesHandler.srcDirectory.getAbsolutePath)
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
              progressBar.value = (loading*100).toInt
          })
          progressBar.visible = false
          val ppController = new PackagePanelController(ag)
          treeDisplayer.contents = Component.wrap(ppController.tree)
          delayedDisplay.foreach(_.visible = true)
        }
        f onSuccess{
          case _ => println("Graph loaded")
        }
    }
    contents += progressBar

    //delayedDisplay+= graphLoader.progressBar

    //val run = makeButton("Eval constraint",
    // "Launch the coupling constraint evaluation")

    val show = makeButton("Show graph",
      "Display a visual representation of the graph without evaluating the constraint"){
      () =>
        Future {
          println("Printing dot ...")
          filesHandler.makeDot ()
          println("Dot printing finished")
          print("dot2png ...")
/*          if(filesHandler.dot2png() ==0)
            println(" success")
          else
            println(" fail")

          val imgframe = ImageFrame(new File(filesHandler.graph.getCanonicalPath + ".png"))
          imgframe.visible = true*/

          val pipedOutput = new PipedOutputStream()
          val pipedInput = new PipedInputStream(pipedOutput)

          Future {
            val imgframe = ImageFrame(pipedInput)
            imgframe.visible = true
          }

          if(filesHandler.dot2png(soutput = Some(pipedOutput)) ==0)
            println(" success")
          else
            println(" fail")


        }

    }
    show.visible = false
    contents +=show
    delayedDisplay += show

  }

  rightComponent = treeDisplayer


}
