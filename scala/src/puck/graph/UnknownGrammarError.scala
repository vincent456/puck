package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */
class UnknownGrammarError(msg:String) extends Error(msg){
  def this()=this("")
}
