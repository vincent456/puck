package puck.javaGraph


import javax.swing.JPopupMenu

import org.piccolo2d.extras.PFrame
import piccolo.{PiccoloDynamicSquareZoomTest, PiccoloTest}
import puck.{GraphStack, JavaIcons}
import puck.piccolo.DGCanvas

import scala.swing.Publisher

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
    val gs = new GraphStack(new Publisher(){})
    gs.pushGraph(bs.graph)
    new PFrame("ExpanseBridge", false,
      new DGCanvas(gs, JavaIcons, (_,_,_,_) => new JPopupMenu()))
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