package puck.graph

/**
 * Created by lorilan on 27/05/14.
 */
class AGError(msg:String) extends Error(msg){
  def this()=this("")
}

class RedirectionError(msg : String) extends AGError(msg){
  def this()=this("")
}

class IllegalAGOperation(msg : String) extends AGError(msg){
  def this() = this("")
}
class AGBuildingError(msg:String) extends AGError(msg){
  def this()=this("")
}
