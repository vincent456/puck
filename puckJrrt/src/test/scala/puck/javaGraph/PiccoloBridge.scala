package puck.javaGraph

import org.piccolo2d.extras.PFrame
import piccolo.PiccoloTest
import puck.piccolo.ClassLayoutPNode

/**
  * Created by lorilan on 4/29/16.
  */
object PiccoloBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    new PiccoloTest(bs.graph)
  }
}

object PiccoloClassTest {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    import bs._


    new PFrame("HierarchyZoomExample", false, null) {
      override def initialize() : Unit = {
        val c = ClassLayoutPNode.createClass(graph, "screen.Screen")
        getCanvas.getLayer.addChild(c)
      }
    }
  }
}