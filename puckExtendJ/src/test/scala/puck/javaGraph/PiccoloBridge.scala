package puck.javaGraph

import puck.ignore
import org.piccolo2d.extras.PFrame
import piccolo.{PiccoloDynamicSquareZoomTest, PiccoloTest}
import puck.JavaIcons
import puck.gui.PuckControl
import puck.javaGraph.stories.BridgeScenario
import puck.piccolo.DGCanvas


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
    val ctrl = new PuckControl(puck.jastadd.ExtendJGraphUtils, JavaIcons, bs.logger)
    ctrl.graphStack setInitialGraph bs.graph
    ignore(new PFrame("ExpanseBridge", false,
      new DGCanvas(ctrl, JavaIcons)))
  }
}
