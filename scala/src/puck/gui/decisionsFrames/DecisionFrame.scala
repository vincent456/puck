package puck.gui.decisionsFrames

import scala.swing.Frame
import java.awt.Dimension
import scala.concurrent.Promise
import scala.util.Success

/**
 * Created by lorilan on 06/06/14.
 */
class DecisionFrame[T] extends Frame{

  size = new Dimension(300, 200)

  val promise = Promise[T]()

  def result = promise.future

  import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
  peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)

  def complete(res : T) {
    promise.complete(Success(res))
    import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
    this.peer.setDefaultCloseOperation(DISPOSE_ON_CLOSE)
    this.close()
  }

}
