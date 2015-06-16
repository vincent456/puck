package puck

/**
 * Created by lorilan on 2/23/15.
 */

object PuckError{
  def apply(msg : String) = new PuckError(msg)
}

class PuckError(msg:String) extends Error(msg){
  def this()=this("")
}
