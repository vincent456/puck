package puck.javaAG

import puck.graph.DGError

/**
 * Created by lorilan on 23/07/14.
 */
class JavaAGError(msg:String) extends DGError(msg) {
  def this()=this("")
}
