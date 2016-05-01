package puck.javaGraph

import piccolo.PiccoloTest

/**
  * Created by lorilan on 4/29/16.
  */
object PiccoloBridge {
  def main(args : Array[String]) : Unit =  {
    val bs = BridgeScenario()
    new PiccoloTest(bs.graph)
  }
}
