package puck.graph.mutable.constraints

/**
 * Created by lorilan on 19/06/14.
 */
class SolvingError(msg : String) extends Error(msg){
  def this() = this("")
}

class Unsolved extends SolvingError
