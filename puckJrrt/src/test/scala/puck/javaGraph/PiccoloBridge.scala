package puck.javaGraph


import org.piccolo2d.extras.PFrame
import piccolo.{PiccoloDynamicSquareZoomTest, PiccoloTest}
import puck.gui.PuckControl
import puck.JavaIcons
import puck.piccolo.{DGCanvas, PiccoloNodeMenu}


/**
  * Created by lorilan on 4/29/16.
  */
object PiccoloBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    puck.ignore(new PiccoloTest(bs.graph))
  }
}

object PiccoloDynamicBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    puck.ignore(new PiccoloDynamicSquareZoomTest(bs.graph, JavaIcons))
  }
}

object ExpanseBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    val ctrl = new PuckControl(puck.jastadd.ExtendJGraphUtils, bs.logger)
    ctrl.graphStack pushGraph bs.graph
    new PFrame("ExpanseBridge", false,
      new DGCanvas(ctrl.graphStack, JavaIcons, PiccoloNodeMenu(ctrl,JavaIcons)))
  }
}
