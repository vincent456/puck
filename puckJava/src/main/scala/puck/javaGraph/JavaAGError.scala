package puck.javaGraph

import puck.graph.DGError

class JavaAGError(msg:String) extends DGError(msg) {
  def this()=this("")
}
