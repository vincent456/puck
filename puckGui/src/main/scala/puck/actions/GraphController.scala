package puck.actions

import puck.GraphStack
import puck.graph._

/**
  * Created by lorilan on 15/12/15.
  */
trait GraphController extends GraphStack {
  val graphUtils : GraphUtils

  def selectedNodes: List[NodeId]
  def selectedEdge : Option[NodeIdP]

}
