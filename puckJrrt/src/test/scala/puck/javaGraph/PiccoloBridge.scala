package puck.javaGraph


import piccolo.{PiccoloDynamicBuildTest, PiccoloTest}

/**
  * Created by lorilan on 4/29/16.
  */
object PiccoloBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    new PiccoloTest(bs.graph)
  }
}

object PiccoloDynamicBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    new PiccoloDynamicBuildTest(bs.graph)
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