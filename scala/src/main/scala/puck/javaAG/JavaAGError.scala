package puck.javaAG

import puck.graph.AGError

/**
 * Created by lorilan on 23/07/14.
 */
class JavaAGError(msg:String) extends AGError(msg) {
  def this()=this("")
}
