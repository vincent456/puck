package puck.gui.search.decisionsFrames

import scala.swing.Frame
import java.awt.Dimension
import scala.concurrent.{Await, Promise}
import scala.util.Success
import scala.concurrent.duration.Duration


/**
 * Created by lorilan on 06/06/14.
 */

object DecisionFrame{
  def apply[T](frameCtor : () => DecisionFrame[T]) : T = {
    val frame = frameCtor()
    frame.visible = true
    Await.result(frame.result, Duration.Inf)
  }
}

class DecisionFrame[T] extends Frame {

  size = new Dimension(300, 200)

  centerOnScreen()

  val promise = Promise[T]()

  def result = promise.future

  import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
  peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

  def complete(res : T) : Unit =  {
    promise.complete(Success(res))
    import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
    this.peer.setDefaultCloseOperation(DISPOSE_ON_CLOSE)
    this.close()
  }
}
