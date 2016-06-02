package puck.javaGraph


import org.piccolo2d.extras.PFrame
import piccolo.{PiccoloDynamicSquareZoomTest, PiccoloTest}
import puck.JavaIcons
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
    new PFrame("ExpanseBridge", false, new DGCanvas(bs.graph, JavaIcons))
  }
}
//object PiccoloClassTest {
//  def main(args : Array[String]) : Unit =  {
//    val bs = BridgeScenario()
//    import bs._
//
//
//    new PFrame("HierarchyZoomExample", false, null) {
//      override def initialize() : Unit = {
//        val c = TypeDeclShapedPNode.createClass(graph, "screen.Screen")
//        getCanvas.getLayer.addChild(c)
//      }
//    }
//  }
//}