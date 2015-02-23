package puck.gui.imageDisplay

import java.awt.Toolkit
import java.io.{PipedInputStream, PipedOutputStream}
import javax.imageio.ImageIO
import javax.swing.ImageIcon

import puck.graph._
import puck.graph.io.VisibilitySet
import puck.gui.PuckMainPanel
import puck.search.SearchState
import puck.util.PuckLog

import scala.concurrent.Future
import scala.swing.BorderPanel.Position
import scala.swing._
import scala.util.{Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by lorilan on 16/11/14.
 */
class ImageExplorer(val filesHandler : FilesHandler,
                    val states : IndexedSeq[SearchState[ResultT]],
                    visibility : VisibilitySet,
                     printId : Boolean,
                     printSignature : Boolean) extends Frame{

  visible = true

  val imageWrapper = new ScrollPane()

  /*{
    val screenSize = Toolkit.getDefaultToolkit.getScreenSize
    size = new Dimension(screenSize.getWidth.toInt, screenSize.getHeight.toInt)
  }*/

  size = new Dimension(PuckMainPanel.width, PuckMainPanel.height)

  val numberStates = states.size
  var index = numberStates -1

  def setImage(): Unit ={
    val state = states(index)
    ImageExplorer.this.title = index + " : " + state.uuid()

    val pipedOutput = new PipedOutputStream()
    val pipedInput = new PipedInputStream(pipedOutput)
    Future {
      ImageIO.read(pipedInput)
    } onComplete {
      case Success(img) =>
        filesHandler.logger.writeln("printing image success")
        imageWrapper.contents = Component.wrap(new ScrollablePicture(new ImageIcon(img),2))
      case _ =>
        filesHandler.logger.writeln("printing image failure")((PuckLog.NoSpecialContext, PuckLog.Error))
    }

    filesHandler.makePng(graphOfResult(state.result),
      visibility,
      printId,
      printSignature,
      None,
      sOutput = Some(pipedOutput))()

  }
  setImage()

  def iButton(i : Int) = new Button(){
    action = new Action(i.toString) {
      def apply() : Unit = {
        index = i
        setImage()
      }
    }
  }


  contents = new BorderPanel{
    val menu = new BoxPanel(Orientation.Horizontal){
      contents += Swing.HGlue
      contents += new Button(){
        action = new Action("<") {
          def apply() : Unit = {
            index = (index - 1) % numberStates
            setImage()
          }
        }
      }
      for( i <- 0 until states.size){
        contents += iButton(i)
      }
      contents += new Button(){
        action = new Action(">") {
          def apply() : Unit = {
            index = (index + 1) % numberStates
            setImage()
          }
        }
      }
      contents += Swing.HGlue
    }
    add(imageWrapper, Position.Center)
    add(menu, Position.South)
  }

}
