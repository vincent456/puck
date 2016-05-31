package puck.javaGraph


import piccolo.{PiccoloDynamicSquareZoomTest, PiccoloTest, TitleNodeExpanseTest}
import puck.JavaIcons

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
    puck.ignore(new TitleNodeExpanseTest(bs.graph, JavaIcons))
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