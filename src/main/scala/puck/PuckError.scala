package puck

/**
 * Created by lorilan on 2/23/15.
 */
class PuckError(msg:String) extends Error(msg){
  def this()=this("")
}
