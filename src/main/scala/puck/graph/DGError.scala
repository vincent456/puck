package puck.graph

import puck.PuckError

/**
 * Created by lorilan on 27/05/14.
 */
class DGError(msg:String) extends PuckError(msg){
  def this()=this("")
}

class RedirectionError(msg : String) extends DGError(msg){
  def this()=this("")
}

class IllegalAGOperation(msg : String) extends DGError(msg){
  def this() = this("")
}
class AGBuildingError(msg:String) extends DGError(msg){
  def this()=this("")
}

class SolvingError(msg : String) extends DGError(msg){
  def this() = this("")
}

class Unsolved extends SolvingError