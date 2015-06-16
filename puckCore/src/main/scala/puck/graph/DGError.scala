package puck.graph

import puck.PuckError

class DGError(msg:String) extends PuckError(msg){
  def this()=this("")
}

class RedirectionError(msg : String) extends DGError(msg){
  def this()=this("")
}

class IllegalAGOperation(msg : String) extends DGError(msg){
  def this() = this("")
}
class DGBuildingError(msg:String) extends DGError(msg){
  def this()=this("")
}

class SolvingError(msg : String) extends DGError(msg){
  def this() = this("")
}

class Unsolved extends SolvingError