package puck

import scala.swing._
import javax.swing.UIManager
/**
 * Created by lorilan on 08/05/14.
 */
object Front extends SwingApplication{

  override def startup(args: Array[String]){
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true
  }
  def top = new MainFrame {
    title = "Puck"

    size = new Dimension(300, 200)

    contents = new Button {
      text = "Click me"
    }
  }
}
